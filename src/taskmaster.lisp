(in-package #:hunchentoot-recycle)

(defvar *default-initial-thread-count* 2)
(defvar *default-max-worker-count* 8)

(defclass recycling-taskmaster (hunchentoot:one-thread-per-connection-taskmaster)
  (;; Overwrites
   (hunchentoot::acceptor-process
    :type hash-table
    :initform (make-hash-table :test 'equal)
    :documentation "recycling-taskmaster tracks processes using a hash-table.")
   (hunchentoot::worker-thread-name-format
    :initform "hunchentoot-parallel-acceptor-~A:~A"
    :documentation "Overriden for parallel-acceptor.")
   ;; new slots.
   (acceptor-process-lock
    :initform (hunchentoot::make-lock "recycling-taskmaster-acceptor-process-lock")
    :reader recycling-taskmaster-acceptor-process-lock
    :documentation "A lock for protecting acceptor-process slot.")
   (shutdown-queue
    :initform (hunchentoot::make-condition-variable)
    :reader recycling-taskmaster-shutdown-queue
    :documentation
    "A condition variable to wait for all threads end, locked by acceptor-process-lock.")
   (initial-thread-count
    :type integer
    :initarg :initial-thread-count
    :initform *default-initial-thread-count*
    :accessor recycling-taskmaster-initial-thread-count
    :documentation 
    "The number of how many threads created at first or off-peak.")
   (max-worker-count
    :type (or null integer) 
    :initarg :max-worker-count
    :initform *default-max-worker-count*
    :accessor recycling-taskmaster-max-worker-count
    :documentation 
    "The number of how many workers may be created at peak. If this
exceeds `hunchentoot:taskmaster-max-thread-count' or
`hunchentoot:taskmaster-max-accept-count', exceeded workers behave
like the original hunchentoot.  If this is NIL, there is no limit
except hunchentoot's limits by above variables. ")
   (parallel-acceptor-handling-thread-count
    :type integer
    :initform 0
    :accessor recycling-taskmaster-parallel-acceptor-handling-thread-count
    :documentation
    "The number of threads made by recycling-taskmaster and working on `hunchentoot:handle-incoming-connection'.")
   (parallel-acceptor-handling-thread-count-lock
    :initform (hunchentoot::make-lock "recycling-taskmaster-parallel-acceptor-handling-thread-count")
    :reader recycling-taskmaster-parallel-acceptor-handling-thread-count-lock
    :documentation
    "A lock for atomically incrementing/decrementing the parallel-acceptor-handling-thread-count-lock slot."))
  (:documentation "(stub)

Thread states:
- :accepting
  Waiting on the listen socket.
  (in `hunchentoot:accept-connections', out of `hunchentoot:handle-incoming-connection').

- :housekeeping
  Checking thread numbers.
  (in `hunchentoot:handle-incoming-connection', out of `hunchentoot:handle-incoming-connection%').
  Counted by `parallel-acceptor-handling-thread-count', but not by hunchentoot's counters.

- :handling-request
  Handling connection
  (in `hunchentoot:handle-incoming-connection%').
  Counted by hunchentoot's original slots,`hunchentoot::thread-count' and `hunchentoot::accept-count'.
"))

(defmethod cl:initialize-instance :after ((taskmaster recycling-taskmaster) &rest init-args)
  "If INITIAL-THREAD-COUNT is supplied, ensure it is equal or less than other count parameters."
  (declare (ignore init-args))
  (with-accessors ((initial recycling-taskmaster-initial-thread-count)
                   (max-worker-count recycling-taskmaster-max-worker-count)
                   (max-thread-count hunchentoot:taskmaster-max-thread-count)
                   (max-accept-count hunchentoot:taskmaster-max-accept-count))
      taskmaster
    (check-type initial integer)
    (when (and max-worker-count
               (not (<= initial max-worker-count)))
      (hunchentoot:parameter-error "INITIAL-THREAD-COUNT must be equal or less than MAX-WORKER-COUNT"))
    (when (and max-thread-count
               (not (<= initial max-thread-count)))
      (hunchentoot:parameter-error "INITIAL-THREAD-COUNT must be equal or less than MAX-THREAD-COUNT"))
    (when (and max-accept-count
               (not (<= initial max-accept-count)))
      (hunchentoot:parameter-error "INITIAL-THREAD-COUNT must be equal or less than MAX-ACCEPT-COUNT"))))

(defmethod increment-recycling-taskmaster-parallel-acceptor-handling-thread-count ((taskmaster recycling-taskmaster))
  (hunchentoot::with-lock-held ((recycling-taskmaster-parallel-acceptor-handling-thread-count-lock taskmaster))
    (incf (recycling-taskmaster-parallel-acceptor-handling-thread-count taskmaster))))

(defmethod decrement-recycling-taskmaster-parallel-acceptor-handling-thread-count ((taskmaster recycling-taskmaster))
  (hunchentoot::with-lock-held ((recycling-taskmaster-parallel-acceptor-handling-thread-count-lock taskmaster))
    (decf (recycling-taskmaster-parallel-acceptor-handling-thread-count taskmaster))))

(defmethod recycling-taskmaster-thread-state (taskmaster thread)
  (hunchentoot::with-lock-held ((recycling-taskmaster-acceptor-process-lock taskmaster))
    (gethash thread (hunchentoot::acceptor-process taskmaster))))

(defmethod (setf recycling-taskmaster-thread-state) (state taskmaster thread)
  (hunchentoot::with-lock-held ((recycling-taskmaster-acceptor-process-lock taskmaster))
    (setf (gethash thread (hunchentoot::acceptor-process taskmaster)) state)))

(defmethod remove-recycling-taskmaster-thread-state (taskmaster thread)
  (hunchentoot::with-lock-held ((recycling-taskmaster-acceptor-process-lock taskmaster))
    (let* ((table (hunchentoot::acceptor-process taskmaster))
           (deleted? (remhash thread table)))
      (when (and deleted?
                 (<= (hash-table-count table) 0))
        (hunchentoot::condition-variable-signal (recycling-taskmaster-shutdown-queue taskmaster)))
      deleted?)))

(defmethod count-recycling-taskmaster-thread (taskmaster)
  (hunchentoot::with-lock-held ((recycling-taskmaster-acceptor-process-lock taskmaster))
    (hash-table-count (hunchentoot::acceptor-process taskmaster))))

(define-condition end-of-parallel-acceptor-thread (condition)
  ()
  (:documentation "Thrown when parallel-acceptor-thread ends."))

(defmethod acceptor-shutdown-p-synchronized (acceptor)
  (hunchentoot::with-lock-held
      ((hunchentoot::acceptor-shutdown-lock acceptor))
    (hunchentoot::acceptor-shutdown-p acceptor)))

(defmethod make-parallel-acceptor-thread ((taskmaster recycling-taskmaster))
  "Makes a new thread for `parallel-acceptor'."
  (let* ((acceptor (hunchentoot:taskmaster-acceptor taskmaster))
         (name (format nil (hunchentoot::taskmaster-worker-thread-name-format taskmaster)
                       (or (hunchentoot:acceptor-address acceptor) "*")
                       (hunchentoot:acceptor-port acceptor))))
    (flet ((thunk ()
             (handler-bind
                 ((end-of-parallel-acceptor-thread
                    (lambda (&optional e)
                      (return-from thunk e)))
                  (error
                    (lambda (&optional e)
                      (when (and
                             (acceptor-shutdown-p-synchronized acceptor)
                             (let ((sock (hunchentoot::acceptor-listen-socket acceptor)))
                               (or (null sock) ; may be nil if already closed.
                                   (not (open-stream-p sock)))))
                        ;; Here, our server was shutdown so the listen
                        ;; socket was closed by the one of
                        ;; parallel-acceptors.  accept(2) to a closed
                        ;; socket may cause EBADF.
                        (return-from thunk e))
                      ;; otherwise, decline.
                      )))
               (unwind-protect
                    (hunchentoot:accept-connections acceptor)
                 (remove-recycling-taskmaster-thread-state taskmaster (bt:current-thread))))))
      (let ((thread (hunchentoot:start-thread taskmaster #'thunk :name name)))
        (setf (recycling-taskmaster-thread-state taskmaster thread) :accepting)))))

(defmethod hunchentoot:execute-acceptor ((taskmaster recycling-taskmaster))
  "Make initial threads working on `parallel-acceptor'."
  (loop repeat (recycling-taskmaster-initial-thread-count taskmaster)
        do (make-parallel-acceptor-thread taskmaster)))

(defmethod hunchentoot:handle-incoming-connection ((taskmaster recycling-taskmaster) client-connection)
  "This function is the core of `recycling-taskmaster'. It is called
 in the loop of `hunchentoot:accept-connections' on every
 accept(2). It processs client-connection by itself and controls how
 many threads working around the process. "
  (unwind-protect
       (let ((handling-threads
               (increment-recycling-taskmaster-parallel-acceptor-handling-thread-count taskmaster))
             (all-threads
               (count-recycling-taskmaster-thread taskmaster))
             (acceptor (hunchentoot::taskmaster-acceptor taskmaster)))
         ;; Makes a thread if all threads are busy.
         (when (and (<= all-threads handling-threads)
                    (if-let ((max-worker (recycling-taskmaster-max-worker-count taskmaster)))
                      (< all-threads max-worker)
                      t)
                    (if-let ((max-thread (hunchentoot:taskmaster-max-thread-count taskmaster)))
                      (< all-threads max-thread)
                      t)
                    (if-let ((max-accept (hunchentoot:taskmaster-max-accept-count taskmaster)))
                      (< all-threads max-accept)
                      t)
                    (not (hunchentoot::acceptor-shutdown-p acceptor))) 
           (make-parallel-acceptor-thread taskmaster))
         ;; process the connection by itself.
         (hunchentoot::handle-incoming-connection% taskmaster client-connection)
         ;; See waiters to determine whether this thread is recyclied or not.
         (setf all-threads
               (count-recycling-taskmaster-thread taskmaster)
               handling-threads
               (recycling-taskmaster-parallel-acceptor-handling-thread-count taskmaster))
         (cond
           ((hunchentoot::acceptor-shutdown-p acceptor)
            (signal 'end-of-parallel-acceptor-thread))
           ((<= all-threads handling-threads)
            ;; There may be a pending connections. This thread should handle it now!
            (progn))
           (t
            ;; Other threads might be waiting on the listen socket besides this thread.
            ;; Assuming there are no pending requests, this thread may exit.
            (when (< (recycling-taskmaster-initial-thread-count taskmaster) all-threads)
              (signal 'end-of-parallel-acceptor-thread)))))
    (decrement-recycling-taskmaster-parallel-acceptor-handling-thread-count taskmaster)))

(defun wake-acceptor-for-shutdown-using-listen-socket (listen-socket)
  "Works like `hunchentoot::wake-acceptor-for-shutdown', except 
 takes the listen socket as an argument and utilize `usocket:socket-shutdown'."
  (multiple-value-bind (address port) (usocket:get-local-name listen-socket)
    (let ((conn (usocket:socket-connect
                 (cond
                   ((and (= (length address) 4) (zerop (elt address 0)))
                    #(127 0 0 1))
                   ((and (= (length address) 16)
                         (every #'zerop address))
                    #(0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1))
                   (t address))
                 port)))
      ;; Use `socket-shutdown' not to block on reading this socket from the server side.
      (usocket:socket-shutdown conn :io)
      (usocket:socket-close conn))))

(defmethod hunchentoot:shutdown ((taskmaster recycling-taskmaster))
  "Tell every workers to shutdown."
  ;; 1. Sets a flag saying "end itself" to the worker threads.
  ;;    -> done by `hunchentoot:stop' setting `hunchentoot::acceptor-shutdown-p'
  ;; 
  ;; 2. Wakes every threads waiting the listen socket.
  ;;    -> utilize `hunchentoot::wake-acceptor-for-shutdown' here.
  ;; 
  ;; NOTE: I saw shutdown(2) can be usable for this purpose:
  ;;    https://stackoverflow.com/questions/9365282/c-linux-accept-blocking-after-socket-closed
  ;; However, usocket does not permit `usocket:socket-shotdown' to a listen socket.
  ;; Even when I `change-class'ed it, Allegro CL does not permit also.
  (loop
    with acceptor = (hunchentoot:taskmaster-acceptor taskmaster)
    with listen-socket = (hunchentoot::acceptor-listen-socket acceptor)
      initially (unless listen-socket
                  (return-from hunchentoot:shutdown nil))
    repeat (count-recycling-taskmaster-thread taskmaster)
    do (handler-case
           (wake-acceptor-for-shutdown-using-listen-socket listen-socket)
         (error (e)
           (hunchentoot::acceptor-log-message acceptor :error "Wake-for-shutdown connect failed: ~A" e)))))

(defmethod wait-for-recycling-taskmaster-shutdown (taskmaster)
  (hunchentoot::condition-variable-wait
   (recycling-taskmaster-shutdown-queue taskmaster)
   (recycling-taskmaster-acceptor-process-lock taskmaster)))

(defmethod hunchentoot:create-request-handler-thread ((taskmaster recycling-taskmaster) client-connection)
  "This method is never called for `recycling-taskmaster'."
  ;; See `make-parallel-acceptor-thread' instead.
  (declare (ignore client-connection))
  (error "This method is not implemented for recycling-taskmaster."))

;;; Counters are same with hunchentoot, currently. (until I implement a new strategy)
;;; TODO: use atomic-incf / atomic-decf
