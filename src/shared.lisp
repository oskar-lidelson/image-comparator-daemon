;;====================
;;Load dependencies:
;;====================
;;;This next line loads quicklisp (a lisp dependency/package management system)
#-quicklisp
(let ((quicklisp-init "quicklisp/setup.lisp"))
  (when (probe-file quicklisp-init)
    (load quicklisp-init)))

;;If it still hasn't loaded, attempt to load it from the default location in home directory: (For development)
#-quicklisp
(let ((quicklisp-init "~/quicklisp/setup.lisp"))
  (when (probe-file quicklisp-init)
    (load quicklisp-init)))


(ql:quickload :hunchentoot :silent T) ;;Used for setting up the HTTP server for receiving task results.
(ql:quickload :cl-csv :silent T) ;;Used for parsing the incoming config files.
(ql:quickload :drakma :silent T) ;;Used for outgoing HTTP requests to push tasks to workers.
(ql:quickload :sqlite :silent T) ;;Used for storing Task and Workload information.

;;====================
;;Config file structure definition
;;====================

;;These parameters at the top are only defined here because they're the most important ones, so they're the ones that should be most visible:
(defparameter *default-master-port* 21411)
(defparameter *default-worker-port* 21412)
;;;ToDo: To be fetched from specified PREFIX directory (/usr/local/etc/..., or preconfigured)
;;;ToDo: This can contain arbitrary lisp code. A more secure option is to open it, read the sexp in it, and check if it's a config struct.
(defparameter *config-file* "/etc/image-comparator.lisp")
(defparameter *worker-config-file* "/etc/image-comparator-worker.lisp")

;;========================================
;;Worker Descriptor (Describes to the system how to contact a worker process)
;;========================================

(defstruct worker-descriptor
  "Workers are independent processes that operate on tasks.

   A worker has an HTTP endpoint for accepting new tasks.

   When a worker finishes a task, it calls back to the master and reports success.

   This contains all of the information needed to describe to the master how to contact a worker."
  (hostname "localhost")
  (port *default-worker-port*))

;;========================================
;;Task Descriptor (Describes to a worker the specifics of a task/job, and where to report success on completion)
;;========================================

(defstruct task-descriptor
  "A Task is a pair of images that I expect a worker to compare for similarity.

   This task descriptor includes all of the information required for a worker to properly do its job.

   A task descriptor is given to a worker, which then carries out that task before reporting back to the server.

   * The image-a/image-b components just tell the worker where to find the images.
   * The uid is so the master knows which task the worker is talking about when it reports back in.
   * The associated-workload-id lets the master know which workload this task belongs to, in case multiple are ongoing simultaneously.
   * The results-return-port tells the worker what port to use to connect back to the master, in case the user changes the master listen port for any reason."
  (image-a "/dev/zero") (image-b "/dev/zero")
  ;;I'll use strings for ID. Integers work, too, but I can't be sure that my type is long enough to avoid collisions in sqlite3.
  (uid "")
  ;;If this is never set for any reason, it'll throw an error when I try to insert into the database for violating the foreign key constraint.
  (associated-workload-id -1)
  ;;The master might be listening on a nondefault port, so the tasks need to record where the results should go:
  ;;I'll use the address they pick up on the other end to know which address to attempt to connect to.
  (results-return-port *default-master-port*))

;;========================================
;;Config, contains all of the default values for the global config and the explanation of each section.
;;========================================

(defstruct config
  "This config file is loaded in at startup and tells it where all of
the directories are, as well as its worker processes.

   Worker processes are assumed to have their own independent startup process. (systemd)"
  (root-directory "/mnt/image-comparator")

  ;;Config files dropped into this directory are ingested and processed:
  (config-incoming-subdir "config.incoming")

  ;;After ingestion, config files are moved to this directory:
  (config-processing-subdir "config.processing")

  ;;After processing, config files are moved to this directory for archiving:
  (config-processed-subdir "config.processed")

  ;;Used to store workload and task information:
  (db "image-comparator-tasks.sqlite3")

  ;;After processing, output goes into this directory:
  (output-subdir "output")

  ;;If we detect an error (file doesn't exist, etc.), this is where it should go.
  (error-log "error.log")
  
  ;;What port the master listens on to receive callbacks from worker processes:
  (http-listen-port *default-master-port*)
  
  ;;How many seconds we sleep between polling the disk for new workloads:
  (workload-check-sleep-interval 10)
  
  ;;The default config assumes we have only one worker on the same host.
  (workers (list (make-worker-descriptor)))


  ;;Part of the Round-Robin worker selector:
  ;;In the future this will be ripped out into a subclass of a generic Worker Selector
  ;; Which will have method: select-next-worker()
  (current-worker-index 0)

  ;;Override this value to make the master a little less verbose.
  ;;ToDo: I should use a proper logger with verbosity values.
  (debug-mode T))


(defstruct worker-config
  "Config definitions for worker processes"

  (db "image-comparator-worker.sqlite3")
  (http-listen-port *default-worker-port*))

;;These are overwritten if a config file exists, by the lisp contents.
(defparameter *config* (make-config))
(defparameter *worker-config* (make-worker-config))

(defun get-worker-db () (worker-config-db *worker-config*))

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
