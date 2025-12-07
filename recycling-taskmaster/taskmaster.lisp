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
    "The number of how many threads created at first or off-peak.")
   (busy-thread-count-cell
    :initform (bt2:make-atomic-integer)
    :accessor recycling-taskmaster-busy-thread-count-cell
    :documentation
    "The number of threads working on `hunchentoot:handle-incoming-connection'.")
   (busy-thread-count-lock
    :initform (hunchentoot::make-lock "recycling-taskmaster-busy-thread-count-lock")
    :reader recycling-taskmaster-busy-thread-count-lock
    :documentation
    "A lock for atomically incrementing/decrementing the busy-thread-count slot.")
   (busy-thread-count-queue
    :initform (hunchentoot::make-condition-variable)
    :reader recycling-taskmaster-busy-thread-count-queue
    :documentation
    "A condition variable to wait for the ends of client connections, locked by busy-thread-count-lock."))
  (:documentation "A taskmaster works like
`hunchentoot:one-thread-per-connection-taskmaster' except recycling
threads when there is a pending connecton.

MAX-THREAD-COUNT and MAX-ACCEPT-COUNT works same as
`hunchentoot:one-thread-per-connection-taskmaster'.

Thread counter summary:

- `busy-thread-count-cell' counts threads working on the accepted
  connection, in our `hunchentoot:handle-incoming-connection'.

  `hunchentoot:taskmaster-thread-count' and
  `hunchentoot:taskmaster-accept-count' are subsets of this.

- The count of all threads is computed by
    (hash-table-count hunchentoot::acceptor-process).
  Its lower bound is in `INITIAL-THREAD-COUNT' slot.

- Threads waiting on the listen socket can be computed by (-
  all-threads-count busy-thread-count).  They are in
  `hunchentoot:accept-connections' out of
  `hunchentoot:handle-incoming-connection'.
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

(defmethod add-recycling-taskmaster-thread (taskmaster thread)
  (hunchentoot::with-lock-held ((recycling-taskmaster-acceptor-process-lock taskmaster))
    (let ((table (hunchentoot::acceptor-process taskmaster)))
      (setf (gethash thread table) t))))

(defmethod remove-recycling-taskmaster-thread (taskmaster thread)
  (hunchentoot::with-lock-held ((recycling-taskmaster-acceptor-process-lock taskmaster))
    (let* ((table (hunchentoot::acceptor-process taskmaster))
           (deleted? (remhash thread table)))
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

(defmethod make-parallel-acceptor-thread ((taskmaster recycling-taskmaster))
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
               (remove-recycling-taskmaster-thread taskmaster (bt:current-thread)))))
      (let* ((name-sig (format nil "~A:~A" (or (hunchentoot:acceptor-address acceptor) "*")
                               (hunchentoot:acceptor-port acceptor)))
             (name (format nil (hunchentoot::taskmaster-worker-thread-name-format taskmaster)
                           name-sig))
             (thread (hunchentoot:start-thread taskmaster #'thunk :name name)))
        (add-recycling-taskmaster-thread taskmaster thread)))))

(defmethod hunchentoot:execute-acceptor :before ((taskmaster recycling-taskmaster))
  "Checks the type of ACCEPTOR slot, because recycling-taskmaster needs
 a crafted `hunchentoot:accept-connections' not using `usocket:with-server-socket'."
  (check-type (hunchentoot::taskmaster-acceptor taskmaster)
              hunchentoot-recycling-taskmaster:parallel-acceptor))

(defmethod hunchentoot:execute-acceptor ((taskmaster recycling-taskmaster))
  "Make initial threads working on `parallel-acceptor'."
  (loop repeat (recycling-taskmaster-initial-thread-count taskmaster)
        do (make-parallel-acceptor-thread taskmaster)))

(defmethod recycling-taskmaster-busy-thread-count ((taskmaster recycling-taskmaster))
  (bt2:atomic-integer-value (recycling-taskmaster-busy-thread-count-cell taskmaster)))

(defmethod increment-recycling-taskmaster-busy-thread-count ((taskmaster recycling-taskmaster))
  (bt2:atomic-integer-incf (recycling-taskmaster-busy-thread-count-cell taskmaster)))

(defmethod decrement-recycling-taskmaster-busy-thread-count ((taskmaster recycling-taskmaster))
  (let ((rest (bt2:atomic-integer-decf (recycling-taskmaster-busy-thread-count-cell taskmaster))))
    (when (<= rest 0)              ; a kind of double-checked locking.
      (hunchentoot::with-lock-held ((recycling-taskmaster-busy-thread-count-lock taskmaster))
        (when (<= (bt2:atomic-integer-value (recycling-taskmaster-busy-thread-count-cell taskmaster)) 0)
          (hunchentoot::condition-variable-signal (recycling-taskmaster-busy-thread-count-queue taskmaster)))))
    rest))

(defmacro with-counting-busy-thread ((var) taskmaster &body body)
  "Executes BODY by incrementing busy-thread-count of TASKMASTER.
 Its value is bound to VAR."
  (let ((taskmaster_ (gensym)))
    `(let* ((,taskmaster_ ,taskmaster)
            (,var (increment-recycling-taskmaster-busy-thread-count ,taskmaster_)))
       (unwind-protect
            (progn ,@body)
         (decrement-recycling-taskmaster-busy-thread-count ,taskmaster_)))))

(defconstant +minimum-accepting-thread-count+ 1)

(defmethod hunchentoot:handle-incoming-connection ((taskmaster recycling-taskmaster) client-connection)
  "This function is the core of `recycling-taskmaster'. It is called
 in the loop of `hunchentoot:accept-connections' on every
 accept(2). It processes a client-connection by itself and controls
 how many threads working around the process. "
  (usocket:with-connected-socket (client-connection client-connection)
    (with-counting-busy-thread (busy-threads) taskmaster
      ;; Makes a thread if all threads are busy.
      (let* ((all-threads (count-recycling-taskmaster-thread taskmaster))
             (listening-threads (- all-threads busy-threads)))
        (when (< listening-threads +minimum-accepting-thread-count+)
          (make-parallel-acceptor-thread taskmaster)))
      ;; process the connection by itself.
      (hunchentoot::handle-incoming-connection%
       taskmaster
       ;; Pass CLIENT-CONNECTION to the Hunchentoot handler and prevent close() here.
       (shiftf client-connection nil))
      ;; See waiters to determine whether this thread is recyclied or not.
      (let* ((busy-threads (recycling-taskmaster-busy-thread-count taskmaster)) ; reads again.
             (all-threads (count-recycling-taskmaster-thread taskmaster))
             (listening-threads (- all-threads busy-threads)))
        (when (and
               ;; Someone is waiting -- means no pending connections.
               (>= listening-threads +minimum-accepting-thread-count+)
               ;; and there are enough other threads.
               (< (recycling-taskmaster-initial-thread-count taskmaster) all-threads))
          (error 'end-of-parallel-acceptor-thread))))))

(defmethod hunchentoot:create-request-handler-thread ((taskmaster recycling-taskmaster) client-connection)
  "This method is never called for `recycling-taskmaster'. See
`make-parallel-acceptor-thread' instead."
  (declare (ignore client-connection))
  (error "This method is not implemented for recycling-taskmaster."))


;;; Shutdown

(defmethod wait-end-of-busy-threads (taskmaster)
  "Wait until no threads counted by the busy-thread-count slot of TASKMASTER."
  (hunchentoot::with-lock-held ((recycling-taskmaster-busy-thread-count-lock taskmaster))
    (loop while (plusp (recycling-taskmaster-busy-thread-count taskmaster))
          do (hunchentoot::condition-variable-wait
              (recycling-taskmaster-busy-thread-count-queue taskmaster)
              (recycling-taskmaster-busy-thread-count-lock taskmaster)))))

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
  (wait-end-of-busy-threads taskmaster)
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


(defmethod abandon-taskmaster ((taskmaster recycling-taskmaster))
  "Abandon all threads kept in TASKMASTER. This is a last resort
 for handling corrupted taskmaster objects."
  ;; see the table without locking intentionally.
  (let* ((table (hunchentoot::acceptor-process taskmaster))
         (thread-list (hash-table-keys table)))
    (loop for thread in thread-list
          do (typecase thread
               (bt2:thread
                (bt2:error-in-thread thread 'end-of-parallel-acceptor-thread))
               (otherwise               ; bt1 thread object.
                (bt:interrupt-thread thread (lambda () (error 'end-of-parallel-acceptor-thread)))))
             ;; Remove THREAD if it held in TASKMASTER.
          if (remove-recycling-taskmaster-thread taskmaster thread)
            count it)))
