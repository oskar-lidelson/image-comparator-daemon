;;====================
;;Load dependencies:
;;====================
;;;This next line loads quicklisp (a lisp dependency/package management system)
#-quicklisp
(let ((quicklisp-init (merge-pathnames "quicklisp/setup.lisp" (user-homedir-pathname))))
  (when (probe-file quicklisp-init)
    (load quicklisp-init)))

(ql:quickload :hunchentoot :silent T) ;;Used for setting up the HTTP server for receiving task results.
(ql:quickload :cl-csv :silent T) ;;Used for parsing the incoming config files.
(ql:quickload :drakma :silent T) ;;Used for outgoing HTTP requests to push tasks to workers.
(ql:quickload :sqlite :silent T) ;;Used for storing Task and Workload information.
