(defparameter *worker-config*
  (make-worker-config
   ;;Where the master node is located:
   :master-hostname "localhost"
   :master-port *default-master-port*
   :db "image-comparator-worker.sqlite3"
   :http-listen-port *default-worker-port*))
