#!/usr/bin/sbcl --script
;; -*- mode: Lisp; -*-

;;====================
;;Set out package to ensure no namespace conflicts occur
;;====================
(defpackage :image-comparator (:use "CL"))
(in-package :image-comparator)

(load "shared.lisp");;Requires that our working dir contains this. 

;;====================
;;Actually load config file: (This function is called from main()) 
;;====================

(defun load-config-file ()
  (handler-case
      ;;If the config file doesn't exist, nothing happens. It just sets *config* to the defaults.
      (progn ()
	     (setf *worker-config* (make-worker-config))
	     (load *worker-config-file* :if-does-not-exist nil))
    (error (e)
      ;;ToDo: Optionally stop on error.
      ;;ToDo: If the config file doesn't exist, it uses the defaults, and that error should be detected and treated specially. (Not fatal)
      (format t "Unable to load config file (~a): ~a" *worker-config-file* e))))

;;====================
;;Database creation and setup (If it doesn't exist)
;;====================
(defun setup-database ()
  "Create the sqlite database if it doesn't exist, and then create the tables I need if they aren't there.

   If the database and tables exist, this function does absolutely nothing."
  ;;The busy-timeout here is unnecessary. I only have it because I might be editing the DB with the CLI sqlite command.
  ;;Really, though, nothing should ever be reading this DB but this daemon.
  (sqlite:with-open-database (db (get-worker-db) :busy-timeout 100)
    (sqlite:execute-non-query
     db
     (concatenate 'string
		  "create table if not exists tasks ("
		  "id integer primary key autoincrement"
		  ;;Foreign key that references the workload which this task belongs to:
		  " ,associated_workload integer not null"
		  ;;1024bytes is enough for very long UIDs. I only use 20 bytes for mine. 
		  " ,uid varchar(1024)"
		  ;;The filename limit again:
		  " ,image_a varchar(4096)"
		  " ,image_b varchar(4096)"
		  ;;ToDo: Optional parameters should somehow go here.

		  ;;A simple integer enum would have sufficed, but this is more informative for anyone reading the raw database.
		  ;;;Possible values:
  		  ;;;* in-progress
		  ;;;* waiting
		  " ,state varchar(256) default 'waiting'"
		  ;;==========
		  ;;OUTPUT VALUES
		  ;;==========
		  ;;;I may not need to store these here, since I return them straight away.
		  " ,similarity_score real default -1"
		  " ,elapsed_time real default -1"
		  ")"))))

;;====================
;;Work functions
;;====================

(defun store-task-in-database (task)
  "Store the task descriptor in the database as WAITING

   Returns the task given."

  ;;Wait up to ten seconds to open this database, since it might be open in the HTTP server thread, reporting status:
  (handler-case
      (sqlite:with-open-database (db (get-db-path) :busy-timeout (* 10 1000))
	(sqlite:execute-non-query
	 db
	 "insert into tasks (associated_workload, uid, image_a, image_b) values(?,?,?,?)"
	 (task-descriptor-associated-workload-id task)
	 (task-descriptor-uid task)
	 (write-to-string (task-descriptor-image-a task))
	 (write-to-string (task-descriptor-image-b task)))
	task)

    (T (error)
      (log-error (format nil "Failed to store task in database: ~A~%" error)))))

(defun start-webserver ()
  "We need a webserver running on the master so we can receive task results.

  This function starts it in its own thread."
  (debug-print
   "Starting webserver on port ~a~%" (worker-config-http-listen-port *worker-config*))
  (hunchentoot:start
   (make-instance 'hunchentoot:easy-acceptor
		  :port (worker-config-http-listen-port *worker-config*)
		  ;;This here is actually unnecessary, but it's so easy to add it in so why not.
		  :document-root (truename "./www"))))

;;Update the tasks DB, set this task in particular to Done.
(hunchentoot:define-easy-handler
    (report-task-complete
     :uri "/image-comparator/task"
     :default-request-type :post)
    ;;Extract parameters from the POST data:
    ((auth-key         :request-type :post)
     (task             :request-type :post))

  (debug-print "Received task: ~a" task)

  ;;ToDo: When auth-key is valid, ...

  (handler-case 
      (let ((task-in (with-input-from-string (stream task) (read stream))))
	;;Push the task to the database:
	(store-task-in-database task-in))
    (T (error)
      (log-error (format nil "Failed to parse incoming task: ~A~%" error)))))

(defun find-next-waiting-task ()
  "Read the database and find the next task in the queue"
  (handler-case
      (sqlite:with-open-database (db (get-db-path) :busy-timeout (* 10 1000))
	(let ((row (sqlite:execute-single "select * from tasks where state = 'waiting' limit 1")))
	  (let ((id (first row))
		(associated-workload-id (second row))
		(uid (third row))
		(image-a (fourth row))
		(image-b (fifth row))))
	  ;;Generate a task:
	  (make-task-descriptor
	   :uid uid
	   :associated-workload-id associated-workload-id
	   :image-a image-a
	   :image-b image-b)))
    (T (error)
      (log-error (format nil "Unhandled error fetching next waiting task: ~A~%" error))
      nil)))

(defun calculate-similarity-score (image-a image-b &key (similarity-type :color-space))
  "Valid similarity-types are: :color-space, :smudge, ..."
  
  )

(defun operate-on-next-available-task ()
  ;;Find the next waiting task:

  (let ((task (find-next-waiting-task)))
    ;;Finish this task:
    (handler-case
	(when task
	  ;;ToDo: Start benchmark timer. (start-time = now())
	  (debug-print "Operating on task: ~A~%" task)

	  ;;Step 1: Convert both sides to PNG:
	  ;;;ToDo: 'convert' binary path should be in the config.
	  (sb-ext:run-program "/usr/bin/convert" (list (task-descriptor-image-a task) "/tmp/image-a.png") :wait T)
	  (sb-ext:run-program "/usr/bin/convert" (list (task-descriptor-image-b task) "/tmp/image-b.png") :wait T)

	  ;;ToDo: Resize both images to be identical.

	  (let ((score (calculate-similarity-score "/tmp/image-a.png" "/tmp/image-b.png")))
	    (debug-print "score: ~A~%" score))
	  
	  ;;ToDo: End benchmark timer: (end-time = now(), elapsed = end-time - start-time).
	  )
      
      (T (error)
	(log-error (format nil "Unhandled error operating on task: ~A~%" error))))))

(defun main ()
  (debug-print "Main worker entrypoint.~%")
  (load-config-file)
  (setup-database)
  (start-webserver)
  ;;Put this thread to sleep forever:
  (loop do (sleep 1)))

;;====================
;;Actual entrypoint
;;====================
(main)

