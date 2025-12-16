(in-package #:hunchentoot-recycling-taskmaster)

(defvar *default-initial-thread-count* 4)

(defclass recycling-taskmaster (hunchentoot:one-thread-per-connection-taskmaster)
  ((hunchentoot::acceptor-process       ; overwrites
    :type hash-table
    :initform (make-hash-table :test 'equal)
    :documentation "recycling-taskmaster tracks threads using a hash-table.")
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
    "The number of how many threads created at first or off-peak."))
  (:documentation "A taskmaster works like
`hunchentoot:one-thread-per-connection-taskmaster' except recycling
threads when there is a pending connecton.

MAX-THREAD-COUNT and MAX-ACCEPT-COUNT works same as
`hunchentoot:one-thread-per-connection-taskmaster'.

Thread counter summary:

- The count of all threads is computed by
  `count-recycling-taskmaster-thread'.
  Its lower bound is in `INITIAL-THREAD-COUNT' slot.

- `hunchentoot::acceptor-requests-in-progress' is a counter in
  Hunchentoot, counts threads working on a connected socket.
  It is used by `estimate-accepting-thread-count'.

  They are in `hunchentoot:accept-connections' and in
  `hunchentoot::handle-incoming-connection%'.
"))


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

(defmethod add-recycling-taskmaster-thread (taskmaster thread-list)
  (hunchentoot::with-lock-held ((recycling-taskmaster-acceptor-process-lock taskmaster))
    (let ((table (hunchentoot::acceptor-process taskmaster)))
      (loop for thread in thread-list
            do (setf (gethash thread table) t)))))

(defmethod remove-recycling-taskmaster-thread (taskmaster thread-list)
  (hunchentoot::with-lock-held ((recycling-taskmaster-acceptor-process-lock taskmaster))
    (let ((table (hunchentoot::acceptor-process taskmaster))
          (deleted? nil))
      (loop for thread in thread-list
            when (remhash thread table)
              do (setf deleted? t))
      (when (and deleted?
                 (<= (hash-table-count table) 0))
        (hunchentoot::condition-variable-signal (recycling-taskmaster-shutdown-queue taskmaster)))
      deleted?)))

(defmethod count-recycling-taskmaster-thread (taskmaster &key (lock t))
  (if lock
      (hunchentoot::with-lock-held ((recycling-taskmaster-acceptor-process-lock taskmaster))
        (hash-table-count (hunchentoot::acceptor-process taskmaster)))
      (hash-table-count (hunchentoot::acceptor-process taskmaster))))

(define-condition end-of-parallel-acceptor-thread (error)
  ()
  (:documentation "Thrown when parallel-acceptor-thread ends."))

(defmethod make-parallel-acceptor-thread ((taskmaster recycling-taskmaster) count)
  "Makes a new thread for `parallel-acceptor'."
  (let ((acceptor (hunchentoot:taskmaster-acceptor taskmaster)))
    (flet ((thunk ()
             (unwind-protect
                  (handler-case
                      (hunchentoot:accept-connections acceptor)
                    (end-of-parallel-acceptor-thread (e)
                      e)
                    (error (e)
                      (when (and
                             (hunchentoot::with-lock-held
                                 ((hunchentoot::acceptor-shutdown-lock acceptor))
                               (hunchentoot::acceptor-shutdown-p acceptor))
                             (let ((sock (hunchentoot::acceptor-listen-socket acceptor)))
                               (or (null sock) ; may be nil if already closed.
                                   (not (open-stream-p sock)))))
                        ;; Here, our server was shutdown so the listen
                        ;; socket was closed by the one of
                        ;; parallel-acceptors.  accept(2) to a closed
                        ;; socket may cause EBADF.
                        (return-from thunk e))
                      ;; otherwise, rethrow it.
                      (error e)))
               ;; For tracking threads at the end, removing from table is deferred to here.
               (remove-recycling-taskmaster-thread taskmaster (list (bt:current-thread))))))
      (let* ((name-sig (format nil "~A:~A" (or (hunchentoot:acceptor-address acceptor) "*")
                               (hunchentoot:acceptor-port acceptor)))
             (name (format nil (hunchentoot::taskmaster-worker-thread-name-format taskmaster)
                           name-sig)))
        (loop repeat count
              collect (hunchentoot:start-thread taskmaster #'thunk :name name)
                into thread-list
              finally
                 (return
                   (add-recycling-taskmaster-thread taskmaster thread-list)))))))

(defmethod hunchentoot:execute-acceptor :before ((taskmaster recycling-taskmaster))
  "Checks the type of ACCEPTOR slot, because recycling-taskmaster needs
 a crafted `hunchentoot:accept-connections' not using `usocket:with-server-socket'."
  (check-type (hunchentoot::taskmaster-acceptor taskmaster)
              hunchentoot-recycling-taskmaster:parallel-acceptor))

(defmethod hunchentoot:execute-acceptor ((taskmaster recycling-taskmaster))
  "Make initial threads working on `parallel-acceptor'."
  (make-parallel-acceptor-thread taskmaster
                                 (recycling-taskmaster-initial-thread-count taskmaster)))

(defconstant +minimum-accepting-thread-count+ 2)

(defmethod estimate-accepting-thread-count (taskmaster)
  "Estimates how many threads waiting on the listen socket."
  (let* ((all-threads (count-recycling-taskmaster-thread taskmaster))
         (acceptor (hunchentoot::taskmaster-acceptor taskmaster))
         (busy-threads
           (hunchentoot::with-lock-held ((hunchentoot::acceptor-shutdown-lock acceptor))
             (hunchentoot::acceptor-requests-in-progress acceptor))))
    (- all-threads busy-threads)))

(defmethod hunchentoot:handle-incoming-connection ((taskmaster recycling-taskmaster) client-connection)
  "This function is the core of `recycling-taskmaster'. It is called
 in the loop of `hunchentoot:accept-connections' on every
 accept(2). It processes a client-connection by itself and controls
 how many threads working around the process. "
  (usocket:with-connected-socket (client-connection client-connection)
    (let* ((accepting-threads (estimate-accepting-thread-count taskmaster))
           (threads-to-be-made (- +minimum-accepting-thread-count+ accepting-threads)))
      ;; Makes a thread if all threads are busy.
      ;; NOTE:
      ;; If the server was shutdown, threads made below will end soon.
      ;; In this place we can stop making threads by seeing
      ;; `hunchentoot::acceptor-shutdown-p', but it requires locking.
      ;; I gave up locking for getting a score of micro-benchmarking.
      (when (plusp threads-to-be-made)
        (make-parallel-acceptor-thread taskmaster threads-to-be-made))
      ;; process the connection by itself.
      (hunchentoot::handle-incoming-connection%
       taskmaster
       ;; Pass CLIENT-CONNECTION to the Hunchentoot handler and prevent close() here.
       (shiftf client-connection nil))
      ;; See waiters to determine whether this thread is recyclied or not.
      (setf accepting-threads (estimate-accepting-thread-count taskmaster)) ; reads again.
      (when (and
             ;; Someone is waiting -- means no pending connections.
             (>= accepting-threads +minimum-accepting-thread-count+)
             ;; and there are enough other threads.
             (< (recycling-taskmaster-initial-thread-count taskmaster)
                (count-recycling-taskmaster-thread taskmaster)))
        (error 'end-of-parallel-acceptor-thread)))))

(defmethod hunchentoot:create-request-handler-thread ((taskmaster recycling-taskmaster) client-connection)
  "This method is never called for `recycling-taskmaster'. See
`make-parallel-acceptor-thread' instead."
  (declare (ignore client-connection))
  (error "This method is not implemented for recycling-taskmaster."))


;;; Shutdown

(defmethod wait-end-of-handle-incoming-connection% (taskmaster)
  "Wait until no threads counted by the busy-thread-count slot of TASKMASTER."
  ;; This code is derived from a part of the original `hunchentoot:stop' function.
  (let ((acceptor (hunchentoot::taskmaster-acceptor taskmaster)))
    (hunchentoot::with-lock-held ((hunchentoot::acceptor-shutdown-lock acceptor))
      (when (plusp (hunchentoot::acceptor-requests-in-progress acceptor))
        (hunchentoot::condition-variable-wait (hunchentoot::acceptor-shutdown-queue acceptor)
                                              (hunchentoot::acceptor-shutdown-lock acceptor))))))

(defmethod delete-recycling-taskmaster-finished-thread (taskmaster)
  "Delete dead threads kept in TASKMASTER accidentally."
  (hunchentoot::with-lock-held ((recycling-taskmaster-acceptor-process-lock taskmaster))
    (loop
      with table = (hunchentoot::acceptor-process taskmaster)
      for thread being the hash-key of (hunchentoot::acceptor-process taskmaster)
      unless (bt:thread-alive-p thread)
        count it into deleted-cnt
        and do (remhash thread table)
      finally
         (when (and (plusp deleted-cnt)
                    (<= (hash-table-count table) 0))
           (hunchentoot::condition-variable-signal (recycling-taskmaster-shutdown-queue taskmaster)))
         (return (plusp deleted-cnt)))))

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
  "Tell every threads to shutdown."
  ;; 1. Sets a flag saying "end itself" to the threads.
  ;;    -> done by `hunchentoot:stop' setting `hunchentoot::acceptor-shutdown-p'
  ;;
  ;; 2. Waits threads in `hunchentoot:handle-incoming-connection'.
  ;; (wait-end-of-handle-incoming-connection% taskmaster)
  ;; 3. Wakes every threads waiting the listen socket, using
  ;; `wake-acceptor-for-shutdown-using-listen-socket'
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
    while (plusp (count-recycling-taskmaster-thread taskmaster))
    do (or (delete-recycling-taskmaster-finished-thread taskmaster)
           (handler-case
               (wake-acceptor-for-shutdown-using-listen-socket listen-socket)
             (error (e)
               (hunchentoot::acceptor-log-message acceptor :error "Wake-for-shutdown connect failed: ~A" e))))))

(defmethod wait-for-recycling-taskmaster-shutdown (taskmaster)
  "Wait until a recycling-taskmaster specified by TASKMASTER ends."
  (hunchentoot::with-lock-held ((recycling-taskmaster-acceptor-process-lock taskmaster))
    (loop while (plusp (count-recycling-taskmaster-thread taskmaster :lock nil))
          do (hunchentoot::condition-variable-wait
              (recycling-taskmaster-shutdown-queue taskmaster)
              (recycling-taskmaster-acceptor-process-lock taskmaster)))))


(defmethod abandon-taskmaster ((taskmaster recycling-taskmaster) &key (lock t))
  "Abandon all threads kept in TASKMASTER. This is a last resort
 for handling corrupted taskmaster objects."
  (flet ((steal-threads ()
           (prog1 (hash-table-keys (hunchentoot::acceptor-process taskmaster))
             (clrhash (hunchentoot::acceptor-process taskmaster))
             (hunchentoot::condition-variable-signal
              (recycling-taskmaster-shutdown-queue taskmaster)))))
    (loop
      with thread-list
        = (if lock
              (hunchentoot::with-lock-held ((recycling-taskmaster-acceptor-process-lock taskmaster))
                (steal-threads))
              (steal-threads))
      for thread in thread-list
      collect
      (typecase thread
        (bt2:thread
         (bt2:error-in-thread thread 'end-of-parallel-acceptor-thread))
        (otherwise                      ; bt1 thread object.
         (bt:interrupt-thread thread (lambda () (error 'end-of-parallel-acceptor-thread))))))))
