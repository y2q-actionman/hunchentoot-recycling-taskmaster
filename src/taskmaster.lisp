(in-package #:hunchentoot-recycle)

(defvar *default-initial-thread-count* 4)

(defclass recycling-taskmaster (hunchentoot:one-thread-per-connection-taskmaster)
  (;; Overwrites
   (hunchentoot::acceptor-process
    :initform nil
    :documentation "recycling-taskmaster does not track acceptor processes.")
   (hunchentoot::worker-thread-name-format
    :initform "hunchentoot-parallel-acceptor-~A:~A"
    :documentation "Overriden for parallel-acceptor.")
   ;; new slots.
   (initial-thread-count
    :type integer
    :initarg :initial-thread-count
    :initform 1
    :accessor recycling-taskmaster-initial-thread-count
    :documentation 
    "The number of how many threads created at first or off-peak."))
  (:default-initargs
   :initial-thread-count *default-initial-thread-count*))

(defmethod cl:initialize-instance :after ((taskmaster recycling-taskmaster) &rest init-args)
  "If INITIAL-THREAD-COUNT is supplied, ensure it is equal or less than other count parameters."
  (declare (ignore init-args))
  (with-accessors ((initial recycling-taskmaster-initial-thread-count)
                   (max-thread-count hunchentoot:taskmaster-max-thread-count)
                   (max-accept-count hunchentoot:taskmaster-max-accept-count))
      taskmaster
    (check-type initial integer)
    (when (and max-thread-count
               (not (<= initial max-thread-count)))
      (hunchentoot:parameter-error "INITIAL-THREAD-COUNT must be equal or less than MAX-THREAD-COUNT"))
    (when (and max-accept-count
               (not (<= initial max-accept-count)))
      (hunchentoot:parameter-error "INITIAL-THREAD-COUNT must be equal or less than MAX-ACCEPT-COUNT"))))

(defmethod make-parallel-acceptor-thread ((taskmaster recycling-taskmaster))
  "Makes a new thread for `parallel-acceptor'."
  (let* ((acceptor (hunchentoot:taskmaster-acceptor taskmaster))
         (thunk (lambda () (hunchentoot:accept-connections acceptor)))
         (name (format nil (hunchentoot::taskmaster-worker-thread-name-format taskmaster)
                       (or (hunchentoot:acceptor-address acceptor) "*")
                       (hunchentoot:acceptor-port acceptor))))
    (hunchentoot:start-thread taskmaster thunk :name name)))

(defmethod hunchentoot:execute-acceptor ((taskmaster recycling-taskmaster))
  "Make initial threads working on `parallel-acceptor'."
  (loop repeat (recycling-taskmaster-initial-thread-count taskmaster)
        do (make-parallel-acceptor-thread taskmaster)))

(defmethod hunchentoot:handle-incoming-connection ((taskmaster recycling-taskmaster) client-connection)
  "This function is the core of `recycling-taskmaster'. It is called
 in the loop of `hunchentoot:accept-connections' on every
 accept(2). It processs client-connection by itself and controls how
 many threads working around the process. "
  ;; TODO:
  ;; - See how many threads, and increase it if all threads are busy.
  
  ;; process the connection by itself.
  ;; TODO: rewrite `handle-incoming-connection%' to our strategy.
  (hunchentoot::handle-incoming-connection% taskmaster client-connection)

  ;; TODO:
  ;; - terminate itself if too many threads working.
  )

(defmethod hunchentoot:shutdown ((taskmaster recycling-taskmaster))
  "Saying every workers to shutdown."
  ;; TODO:
  ;; 1. Sets a flag saying "end itself" to the worker threads.
  ;; 2. wakes every threads waiting the listen socket (how?)
  ;;    https://stackoverflow.com/questions/9365282/c-linux-accept-blocking-after-socket-closed
  (let* ((acceptor (hunchentoot:taskmaster-acceptor taskmaster))
         (listen-socket (hunchentoot::acceptor-listen-socket acceptor)))
    (usocket:socket-shutdown listen-socket :io))
  ;; TODO: wait for all threads end.
  )

(defmethod hunchentoot:create-request-handler-thread ((taskmaster recycling-taskmaster) client-connection)
  "This method is never called for `recycling-taskmaster'."
  ;; See `make-parallel-acceptor-thread' instead.
  (declare (ignore client-connection))
  (error "This method is not implemented for recycling-taskmaster."))

;;; Counters are same with hunchentoot, currently. (until I implement a new strategy)
;;; TODO: use atomic-incf / atomic-decf
