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
         (name (format nil (hunchentoot::taskmaster-worker-thread-name-format taskmaster)
                       (or (hunchentoot:acceptor-address acceptor) "*")
                       (hunchentoot:acceptor-port acceptor))))
    (flet ((thunk ()
             (handler-bind
                 ((error
                    (lambda (&optional e)
                      (declare (ignore e))
                      (when (and
                             (hunchentoot::acceptor-shutdown-p acceptor)
                             (not (open-stream-p (hunchentoot::acceptor-listen-socket acceptor))))
                        ;; Here, our server was shutdown so the listen
                        ;; socket was closed by the one of
                        ;; parallel-acceptors.  accept(2) to a closed
                        ;; socket may cause EBADF.
                        (return-from thunk nil))
                      ;; otherwise, decline.
                      )))
               (hunchentoot:accept-connections acceptor))))
      (hunchentoot:start-thread taskmaster #'thunk :name name))))

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
  ;; 1. Sets a flag saying "end itself" to the worker threads.
  ;;    -> done by `hunchentoot:stop' setting `hunchentoot::acceptor-shutdown-p'
  ;; 
  ;; 2. Wakes every threads waiting the listen socket.
  ;;    -> utilize `hunchentoot::wake-acceptor-for-shutdown' here.
  ;; 
  ;; I saw shotdown(2) can be usable for this purpose:
  ;;    https://stackoverflow.com/questions/9365282/c-linux-accept-blocking-after-socket-closed
  ;; However, usocket does not permit `usocket:socket-shotdown' to a listen socket.
  ;; Even when I `change-class'ed it, Allegro CL does not permit also.
  (loop
    with acceptor = (hunchentoot:taskmaster-acceptor taskmaster)
    with thread-count = (1- ; because `hunchentoot:stop' calls it once.
                         ;; FIXME: use a *current* value.
                         (recycling-taskmaster-initial-thread-count taskmaster))
    repeat thread-count
    do (hunchentoot::wake-acceptor-for-shutdown acceptor))
  ;; TODO: wait for all threads end.
  )

(defmethod hunchentoot:create-request-handler-thread ((taskmaster recycling-taskmaster) client-connection)
  "This method is never called for `recycling-taskmaster'."
  ;; See `make-parallel-acceptor-thread' instead.
  (declare (ignore client-connection))
  (error "This method is not implemented for recycling-taskmaster."))

;;; Counters are same with hunchentoot, currently. (until I implement a new strategy)
;;; TODO: use atomic-incf / atomic-decf
