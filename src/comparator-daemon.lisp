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
	     (setf *config* (make-config))
	     (load *config-file* :if-does-not-exist nil))
    (error (e)
      ;;ToDo: Optionally stop on error.
      ;;ToDo: If the config file doesn't exist, it uses the defaults, and that error should be detected and treated specially. (Not fatal)
      (format t "Unable to load config file (~a): ~a" *config-file* e))))

;;====================
;;Database creation and setup (If it doesn't exist)
;;====================
;;;At this point in the code, the config file is loaded and we have our database dependencies loaded.
;;;This code runs unconditionally:
(defun setup-database ()
  "Create the sqlite database if it doesn't exist, and then create the tables I need if they aren't there.

   If the database and tables exist, this function does absolutely nothing."
  ;;The busy-timeout here is unnecessary. I only have it because I might be editing the DB with the CLI sqlite command.
  ;;Really, though, nothing should ever be reading this DB but this daemon.
  (sqlite:with-open-database (db (get-db-path) :busy-timeout 100)
    ;;First, generate the Workloads table if it isn't already there:
    ;;;A workload has:
    ;;; * ID (Standard primary key)
    ;;; * Incoming config file name ;; I don't actually need to store this, but it's handy for debugging.
    ;;; * Outgoing results file name ;; This is required. I need to know where to put the output!
    (sqlite:execute-non-query
     db
     (concatenate 'string
		  "create table if not exists workloads ("
		  " id integer primary key autoincrement"
		  ;;You never know what ridiculously long filename a person might use. Better to be prepared.
		  ;;On linux, typically, FILENAME_MAX is 4096 (or 255 on ext2,3,4).
		  ;;I can actually get the value with $(getconf NAME_MAX <dir-on-filesystem>)
		  ;;I also left a misc/filename-max.c program to test it yourself on MacOS or Windows.
		  ;;4096 is a safe value for now.
		  " ,incoming_config_file_name varchar(4096)"
		  " ,outgoing_results_file_name varchar(4096)"
		  ")"))
    ;;ToDo: I should really check that this was successful..
    ;;This code throws an error if it fails, actually, and that stops the process altogether.
    ;;sqlite is special in that, once a create query is correct, it will remain correct forever.
    ;;So I only have to test this code until it works, then just never edit it. Ever.
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
  		  ;;;* ready-to-push
		  ;;;* in-progress
		  ;;;* done
		  " ,state varchar(256) default 'ready-to-push'"
		  ;;==========
		  ;;OUTPUT VALUES
		  ;;==========
		  ;;;These are what come back from the report task complete endpoint.
		  " ,similarity_score real default -1"
		  " ,elapsed_time real default -1"
		  
		  ;;Then the foreign key constraints at the end (sqlite requires they go at the end)
		  " ,foreign key(associated_workload) references workloads(id)"
		  ")"))))

;;====================
;;Utilities
;;====================

(defmacro debug-print (&rest code)
  "Shorthand macro for printing if the global config 'debug-mode' is set to T. Otherwise does nothing."
  `(when (config-debug-mode *config*) (format T ,@code)))


(defun get-db-path ()
  "Get the absolute path to the database"
  (pathname (format nil "~a/~a"
		    (config-root-directory *config*)
		    (config-db *config*))))

(defun get-error-log-path ()
  "Get the absolute path to the error log"
  (pathname (format nil "~a/~a/~a"
		    (config-root-directory *config*)
		    (config-output-subdir *config*)
		    (config-error-log *config*))))

(defun get-processing-subdir-path ()
  "Get the absolute path to the processing subdir"
  (pathname (format nil "~a/~a"
		    (config-root-directory *config*)
		    (config-processing-subdir *config*))))

(defun log-error (error)
  "Given an error, write it to our error log and carry on.

   If it's fatal, the caller will already have begun unwinding."
  (let ((str
	  (with-output-to-string (stream)
	    (format stream "========================================~%")
	    (format stream " ~A~%" error)
	    (format stream "========================================~%"))))
    (debug-print str)
    (with-open-file (stream
		     (get-error-log-path)
		     :direction :output
		     :if-does-not-exist :create
		     :if-exists :append)
      (format stream "~A~%" str))))

(defun mkdir-or-die (directory mode &key (relative nil))
  "Given a directory name, and a mode, attempt to create it.

   If relative is true, it creates it relative to the configured root-directory.

   This ignores error 17: File exists. All other errors go into the error log, and are thrown to the caller."
  (handler-case 
      (sb-posix:mkdir (pathname
		       ;;If relative, be relative to the root dir:
		       (if relative (format nil "~A/~A" (config-root-directory *config*) directory)
			   directory))
		      mode)
    (sb-posix:syscall-error (error)
      (cond
	;;Error code 17: File exists. This is non-fatal. Ignore it.
	((= (sb-posix:syscall-errno error) 17) nil)
	(T (log-error (format nil "Failed to create directory (~A): ~A~%" directory error)))))))

(defun generate-directories-in-config ()
  "This is typically called after the config is loaded. It generates all of the directories required to run this program successfully.

   Those include: 
   * The output dir.
   * The input dirs.
   * The processing and processed dirs."
  (let ((u+rwx/g+rwx/o-rwx #O770))
    ;;This is where we run into problems.
    ;;ToDo: Actually catch each possible error and determine why the mkdir call failed.
    ;; * It already exists <- This is not fatal.
    ;; * Permissions error <- This is fatal.
    ;; * Parent directory doesn't exist <- This is fatal.

    (mkdir-or-die (config-root-directory *config*) u+rwx/g+rwx/o-rwx)
    (mkdir-or-die (config-config-processing-subdir *config*) u+rwx/g+rwx/o-rwx :relative T)
    (mkdir-or-die (config-config-processed-subdir *config*) u+rwx/g+rwx/o-rwx :relative T)
    (mkdir-or-die (config-output-subdir *config*) u+rwx/g+rwx/o-rwx :relative T)
    (mkdir-or-die (config-config-incoming-subdir *config*) u+rwx/g+rwx/o-rwx :relative T)))

(defun worker-to-uri (worker)
  "Better done with classes and the print method, but classes make the config more complex.
   It's better to have a simple config and a slightly less elegant codebase."
  (format nil "http://~a:~a" (worker-descriptor-hostname worker) (worker-descriptor-port worker)))

(defun file-exists (filename)
  "Returns non-NIL if file exists."
  (probe-file (pathname filename)))
(defun file-doesnt-exist (filename)
  "Shorthand."
  (not (file-exists (pathname filename))))

(defun generate-task-uid (&key (num-digits 20))
  "Generate a sensible UID which can be natively handled as an integer in Lisp"
  ;;It's important to use 10 here as the base so that it stays in integer-land.
  (write-to-string (random (expt 10 num-digits))))

(defun incoming-subdir-wildcard (&key (file-type "csv"))
  "Construct the wildcard path which globs to all incoming config files of the specified file type.
   ie. The default value for this should look something like /mnt/.../config.incoming/*.csv"
  (format nil "~a/~a/*.~a"
	  (config-root-directory *config*)
	  (config-config-incoming-subdir *config*)
	  file-type))

(defun basename (path)
  "Emulates the UNIX basename command. (basename(/tmp/x/y) => y)"
  (format nil "~a~a~a"
	  (pathname-name path)
	  (if (pathname-type path) "." "")
	  (if (pathname-type path)
	      (pathname-type path) "")))

(defun move-config-file-to-processing-dir (config-file-name)
  "Given a config-file-name (absolute path) (one of the incoming configs), 

   move it wholesale into the processing subdir."
  (let ((source config-file-name)
	(dest (format nil "~A/~A/~A"
		      (config-root-directory *config*)
		      (config-config-processing-subdir *config*)
		      (basename config-file-name))))
    (format t "Moving ~a to ~A~%" source dest)
    (handler-case
	(sb-posix:rename source dest)
      (T (error)
	(log-error (format nil "Failed to rename '~a' to '~a': ~a~%" source dest error))))))

(defun set-task-status-to-done (db task-uid similarity-score elapsed-time)
  (sqlite:execute-non-query
   db
   "update tasks set state='done' and similarity_score = ? and elapsed_time = ? where uid='?'"
   similarity-score
   elapsed-time
   task-uid))

(defun get-associated-workload-id-for-task (db task-uid)
  (sqlite:execute-single db "select associated_workload from tasks where uid = ?" task-uid))

(defun count-unfinished-tasks-for-workload (db workload-id)
  (sqlite:execute-single db "select count(id) from tasks where associated_workload = ?" workload-id))

(defun workload-has-completed-all-tasks (db workload-id)
  (= 0 (count-unfinished-tasks-for-workload db workload-id)))

(defun associated-workload-has-completed-all-tasks (db task-uid)
  (workload-has-completed-all-tasks
   db
   (get-associated-workload-id-for-task db task-uid)))

;;ToDo: Create generalized task class which auto-generates the ID and stores it in Sqlite database.
;;ToDo: Create generalized worker class which can accept a Task instance. 
(defun push-task-to-worker (worker image-pair-i task-id)
  "Given a worker from the config, and an image pair from the incoming csv, push this task over the network."
  (let ((task-push-URL (format nil "http://~a/image-comparator/task" (worker-to-uri worker))))
    (handler-case
	;;Make an HTTP request to the worker endpoint and push the details of this task over.
	(drakma:http-request task-push-url
			     :method :post
			     :parameters
			     (list
			      (cons "auth-token" "ToDo")
			      (cons "task-id" task-id)
			      ;;Additional parameters from the csv file are also passed:
			      (cons "image-filename-pair" (format nil "~S" image-pair-i))))
      (error (e)
	(format nil "Failed to push task to worker: ~A~%" e)
	;;ToDo: Mark task as unpushed in database.
	nil))))

(defun check-config-line-validity (image-pair-i config-file-name)
  "Given a row from the incoming config file, check that it's valid.

   Also accepts the config-file-name for debug output message purposes.

   Throws an error if it isn't, and otherwise does nothing."
  ;;==========
  ;;Check that it has valid form:
  ;;==========
  ;;First validity test:
	;;; * It has (at least) two entries:
  (when (< (length image-pair-i) 2)
    (error (format nil "Malformed image config in config file (~a)~%  Expecting a pair, but instead got '~a'~%"
		   config-file-name image-pair-i)))
  ;;Second validity test:
	;;; * Both files must exist.
  (flet ((file-existence-check (filename)
	   (when (file-doesnt-exist filename)
	     (error (format nil "Malformed image config in config file (~a)~%  File doesn't exist: ~a~%"
			    config-file-name filename)))))
    (file-existence-check (first image-pair-i))
    (file-existence-check (second image-pair-i)))

	;;;ToDo: Maybe check that both files are images of a supported type?

  ;;==========
  ;;Validity tests passed.
  ;;==========
  T)

(defun validate-successful-push (http-post-result)
  "Given the result of an HTTP POST request to push a task, checks that it was successful.

   If it was, the task is marked as IN-PROGRESS in the database."
  ;;If control reaches this function, it means that the handler-case hadn't unwound and skipped it.
  ;;That means there wasn't any serious error (connection failed, etc.)
  (debug-print "Task pushed to worker. Attempting to mark it as in-progress in database.~%")
  (format T "Result: ~A~%" http-post-result)
  ;;;ToDo.
  )

(defun push-task-to-worker (worker task)
  "Given a worker, and a task, generate that workers URL endpoint for pushing tasks, then make the request.

   After pushing, the result of the HTTP request is passed to #'validate-successful-push, which typically marks the task as IN-PROGRESS in the database."
  (let ((task-push-url (format nil "~a/image-comparator/task"
			       (worker-to-uri worker))))
    (debug-print "Pushing task to endpoint: ~a~%" task-push-url)
    ;;ToDo: Use an auth key here?
    (handler-case
	(validate-successful-push
	 (drakma:http-request task-push-url
			      :method :post
			      :parameters
			      (list
			       (cons "auth-token" "ToDo")
			       ;;Push the task over wholesale.
			       (cons "task" (format nil "~S" task)))))
      (t (error)
	(format T "Failed to push task to worker (~A): ~A~%" task-push-url error)
	nil))))

(defun select-next-worker ()
  "Look into the config and return the next available worker.

   By default, uses Round-Robin task assignment."
  ;;==========
  ;;Round robin task allocation
  ;;==========
  ;;Submit this pair to workers in round-robin fashion:
  ;;A more sophisticated tool would have a worker pool manager which knows immediately which of the workers is free.
	;;;Rotate this index. Nothing wrong with doing it up front.

  ;;Increment the current index circularly up to num-workers: 
  (setf (config-current-worker-index *config*)
	(mod (+ 1 (config-current-worker-index *config*))
	     (length (config-workers *config*))))
  ;;Return the worker at this index:
  (nth (config-current-worker-index *config*)
       (config-workers *config*)))

(defun generate-task-from-config-line (config-line workload-id)
  "We know that config-line is valid at this point.

   Config-line has form:
   * (first c) -> Image-a
   * (second c) -> Image-b
   * ... -> Optional parameters.

   This task is assigned to workload with database ID given by workload-id."
  ;;First, generate a new ID for this task.
  (let ((id (generate-task-uid)))
    (make-task-descriptor :uid id :image-a (first config-line) :image-b (second config-line)
			  :associated-workload-id workload-id
			  ;;What port to use when attempting to connect back to report results.
			  ;;I don't need to store this in the database.
			  :results-return-port (config-http-listen-port *config*))))

(defun store-task-in-database (task)
  "Store the task descriptor in the database as READY-TO-PUSH.

   Returns the task given."

  ;;Wait up to ten seconds to open this database, since it might be open in the HTTP server thread, reporting status:
  (sqlite:with-open-database (db (get-db-path) :busy-timeout (* 10 1000))
    (sqlite:execute-non-query
     db
     "insert into tasks (associated_workload, uid, image_a, image_b) values(?,?,?,?)"
     (task-descriptor-associated-workload-id task)
     (task-descriptor-uid task)
     (write-to-string (task-descriptor-image-a task))
     (write-to-string (task-descriptor-image-b task)))
    task))

(defun finish-workload (db workload-id)
  "Given a database handle (DB) and a workload ID corresponding to one of the workloads in the DB,

   * Copy the config file into the archival directory from the processing directory.
   * Find all of the tasks and write their timings and similarity scores out to the output file."
  
  )


(defun create-new-workload (config-file-name)
  "Given an incoming config file (config-file-name)

   * Create a new workload in the database, storing the config filename.
   * Return its ID."
  (sqlite:with-open-database (db (get-db-path) :busy-timeout (* 10 1000))
    (sqlite:execute-non-query
     db
     "insert into workloads (incoming_config_file_name, outgoing_results_file_name) values(?,?)"
     (write-to-string config-file-name) ;;incoming config file name 
     (format nil "~A.out.csv" config-file-name) ;;outgoing config file name. It's saved with .out.csv appended.
     )
    (sqlite:execute-single db "select last_insert_rowid()")))

(defun operate-on-incoming-config-files (&key (file-type "csv"))
  "Find all incoming config files matching the glob, read them in, and operate."
  (let ((files (directory (incoming-subdir-wildcard :file-type file-type))))
    ;;For each file found, ...
    (dolist (config-file-i files)
      (debug-print "Operating on config file (~a).~%" config-file-i)

      ;;Next, we need to generate a new workload in the database and keep track of its ID:
      (let ((workload-id (create-new-workload config-file-i))
	    ;;ToDo: Read the config file one line at a time to avoid slurping too much into (possibly limited) memory.
	    ;;For now, we read it all at once.
	    (csv-values (cl-csv:read-csv (pathname config-file-i))))

	(debug-print "New workload generated with id (~a)~%" workload-id)

	;;Check if the first line is the heading (this fails if it's spelled or capitalized differently):
	(when (and (string= "image1" (first (first csv-values)))
		   (string= "image2" (second (first csv-values))))
	  ;;If it is a heading, strip it off and let the garbage collector have it.
	  (setf csv-values (cdr csv-values)))

	;;Now iterate over the rest of the rows:
	(dolist (image-pair-i csv-values)

	  (let ((validation-error nil))
	    (handler-case
		(check-config-line-validity image-pair-i config-file-i)
	      (t (error)
		;;Delete this workload and return the error to the caller?
		;;I don't think it makes sense to kill a whole workload just because one of the tasks is impossible.
		;;It should probably be marked with an error.
		(setf validation-error error)
		(log-error error)))

	    (debug-print "Operating on image pair (~a,~a)~%" (first image-pair-i) (second image-pair-i))

	    (unless validation-error
	      ;;Generate a new task, store it in the database, select a worker for it, push that task, marking it as in-progress if successful.
	      (push-task-to-worker (select-next-worker)
				   (store-task-in-database
				    (generate-task-from-config-line image-pair-i workload-id)))
	      )))

	;;Now that the entire CSV file has been read and added to processing, move the config file to the processing directory
	(move-config-file-to-processing-dir config-file-i)
	))))

(defun start-webserver ()
  "We need a webserver running on the master so we can receive task results.

  This function starts it in its own thread."
  (debug-print
   "Starting webserver on port ~a~%" (config-http-listen-port *config*))
  (hunchentoot:start
   (make-instance 'hunchentoot:easy-acceptor
		  :port (config-http-listen-port *config*)
		  ;;This here is actually unnecessary, but it's so easy to add it in so why not.
		  :document-root (truename "./www"))))

;;Update the tasks DB, set this task in particular to Done.
(hunchentoot:define-easy-handler
    (report-task-complete
     :uri "/task/report"
     :default-request-type :post)
    ;;Extract parameters from the POST data:
    ((auth-key         :request-type :post);;ToDo.
     (task-uid         :request-type :post)
     (similarity-score :request-type :post)
     (elapsed-time     :request-type :post)
     (error-message    :request-type :post))

  (debug-print "Reported task complete: ~a" task-uid)

  (unless task-uid
    ;;No task UID presented? This is a serious error.
    ;;ToDo: Log enough information to debug this.
    (log-error "No task uid presented to /task/report."))
  
  (when task-uid
    (sqlite:with-open-database (db (get-db-path) :busy-timeout (* 10 1000))
      ;;Determine if there was an error:
      (when error-message
	(log-error (format nil "Task with uid ~a reported error ~a.~%" task-uid error-message)))

      ;;If there wasn't an error, carry on:
      (unless error-message
	;;Update this task's state and set it to 'done':
	(set-task-status-to-done db task-uid similarity-score elapsed-time)

	;;Determine if there are any tasks left for the associated workload.
	(when (associated-workload-has-completed-all-tasks db task-uid)
	  ;;Finally, copy the config file to the archival directory and begin writing the output.
	  ;;I know that this function is called twice (get-associa...) but I don't mind the slight overhead for a little cleaner code.
	  (finish-workload db (get-associated-workload-id-for-task task-uid)))))))

(defun main ()
  (debug-print "Main entrypoint.~%")
  (load-config-file)
  (generate-directories-in-config)
  (setup-database)
  (start-webserver)
  ;;Forever:
  (loop do
    (operate-on-incoming-config-files)
    (sleep (config-workload-check-sleep-interval *config*))))

;;====================
;;HTTP Server here. 
;;====================
;;;This http server is required. I need to be able to receive task results from the workers. 

;;====================
;;Actual entrypoint
;;====================
(main)

