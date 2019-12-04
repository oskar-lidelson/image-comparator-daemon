(defparameter *config*
  (make-config
   :root-directory "/mnt/image-comparator"

   ;;Config files dropped into this directory are ingested and processed:
   :config-incoming-subdir "config.incoming"

   ;;After ingestion, config files are moved to this directory:
   :config-processing-subdir "config.processing"

   ;;After processing, config files are moved to this directory for archiving:
   :config-processed-subdir "config.processed"

   ;;Used to store workload and task information:
   :db "image-comparator-tasks.sqlite3"

   ;;After processing, output goes into this directory:
   :output-subdir "output"

   ;;If we detect an error (file doesn't exist, etc.), this is where it should go.
   :error-log "error.log"
   
   ;;What port the master listens on to receive callbacks from worker processes:
   :http-listen-port *default-master-port*
   
   ;;How many seconds we sleep between polling the disk for new workloads:
   :workload-check-sleep-interval 10
   
   ;;The default config assumes we have only one worker on the same host.
   :workers (list
	     (make-worker-descriptor
	      :hostname "localhost"
	      :port *default-worker-port*)
	     (make-worker-descriptor
	      :hostname "localhost"
	      :port *default-worker-port*))

   :debug-mode T)
  )
