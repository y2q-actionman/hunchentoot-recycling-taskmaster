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

- `hunchentoot::acceptor-requests-in-progress' is a counter in the
  original Hunchentoot and counts threads working on a connected
  socket.

  They are in `hunchentoot:accept-connections' and in
  `hunchentoot::handle-incoming-connection%'.

Thread states:

1. In `hunchentoot::handle-incoming-connection%'.
   They are counted by `hunchentoot::acceptor-requests-in-progress'.
   `hunchentoot::acceptor-shutdown-queue' can be used to wait for
   they go away.

2. Waiting on the listen socket.
   They can be wake by `wake-acceptor-for-shutdown-using-listen-socket'.
   I estimate the number of them by
     (- `count-recycling-taskmaster-thread'
        `hunchentoot::acceptor-requests-in-progress'),
   but it might contain threads at the third or fourth state (below).

3. At other places in the loop of `hunchentoot:accept-connections'.
   It rarely exists. However, when a last thread was this state just
   before `wake-acceptor-for-shutdown-using-listen-socket', the thread
   would exit soon and
   `wake-acceptor-for-shutdown-using-listen-socket' blocks
   permanently. My `hunchentoot:shutdown' considers this situation.

4. Interrupted and broken, but held by `hunchentoot::acceptor-process'.
   `delete-recycling-taskmaster-finished-thread' deletes them.
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
    (let ((table (hunchentoot::acceptor-process taskmaster)))
      (remhash thread table))))

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

(defmethod count-busy-thread (taskmaster)
  "Estimates how many threads are on client connections."
  (let ((acceptor (hunchentoot::taskmaster-acceptor taskmaster)))
    (hunchentoot::with-lock-held ((hunchentoot::acceptor-shutdown-lock acceptor))
      (hunchentoot::acceptor-requests-in-progress acceptor))))

(defmethod hunchentoot:handle-incoming-connection ((taskmaster recycling-taskmaster) client-connection)
  "This function is the core of `recycling-taskmaster'. It is called
 in the loop of `hunchentoot:accept-connections' on every
 accept(2). It processes a client-connection by itself and controls
 how many threads working around the process. "
  (usocket:with-connected-socket (client-connection client-connection)
    (let* ((all-threads (count-recycling-taskmaster-thread taskmaster))
           (busy-threads (count-busy-thread taskmaster))
           (accepting-threads (- all-threads busy-threads)))
      ;; Makes a thread if all threads are busy.
      ;; NOTE:
      ;; If the server was shutdown, threads made below will end soon.
      ;; In this place we can stop making threads by seeing
      ;; `hunchentoot::acceptor-shutdown-p', but it requires locking.
      ;; I gave up locking for getting a score of micro-benchmarking.
      ;; NOTE:
      ;; I've tried to adjust how many threads made here, like
      ;; calculate "(- +how-many-threads-waiting-on-the-listen-socket+
      ;; accepting-threads)". but I could not get remarkable
      ;; changes. So I decided to fix it to '1'.
      (when (<= accepting-threads 0)
        (make-parallel-acceptor-thread taskmaster)))
    ;; process the connection by itself.
    (hunchentoot::handle-incoming-connection%
     taskmaster
     ;; Pass CLIENT-CONNECTION to the Hunchentoot handler and prevent close() here.
     (shiftf client-connection nil))
    ;; See waiters to determine whether this thread is recyclied or not.
    (let ((all-threads (count-recycling-taskmaster-thread taskmaster)) ; reads again.
          (initial-threads (recycling-taskmaster-initial-thread-count taskmaster)))
      (when (and
             ;; There are enough other threads.
             (< initial-threads all-threads)
             ;; and someone is waiting -- means no pending connections.
             (let* ((busy-threads (count-busy-thread taskmaster))
                    (accepting-threads (- all-threads busy-threads)))
               (plusp accepting-threads)))
        (error 'end-of-parallel-acceptor-thread)))))

(defmethod hunchentoot:create-request-handler-thread ((taskmaster recycling-taskmaster) client-connection)
  "This method is never called for `recycling-taskmaster'. See
`make-parallel-acceptor-thread' instead."
  (declare (ignore client-connection))
  (hunchentoot::not-implemented 'hunchentoot:create-request-handler-thread))


;;; Shutdown

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
         (return (plusp deleted-cnt)))))

(defconstant +wake-acceptor-for-shutdown-timeout+ 1
  "The timeout used by
 `wake-acceptor-for-shutdown-using-listen-socket' to avoid an
 accidental permanent block in our `hunchentoot:shutdown'.")

(defun wake-acceptor-for-shutdown-using-listen-socket (listen-socket &key (timeout +wake-acceptor-for-shutdown-timeout+))
  "Works like `hunchentoot::wake-acceptor-for-shutdown', except takes
 the listen socket as an argument, apply a timeout, and utilize
 `usocket:socket-shutdown'."
  (multiple-value-bind (address port) (usocket:get-local-name listen-socket)
    (let ((conn (usocket:socket-connect
                 (cond
                   ((and (= (length address) 4) (zerop (elt address 0)))
                    #(127 0 0 1))
                   ((and (= (length address) 16)
                         (every #'zerop address))
                    #(0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1))
                   (t address))
                 port
                 :timeout timeout :nodelay t)))
      ;; Use `socket-shutdown' not to block on reading this socket from the server side.
      (usocket:socket-shutdown conn :io)
      (usocket:socket-close conn))))

(defconstant +wake-acceptor-for-shutdown-max-retry-count+ 3
  "How many times `hunchentoot:shutdown' retries
  `wake-acceptor-for-shutdown-using-listen-socket'.")

(define-condition recycling-taskmaster-shutdown-aborted-warning (hunchentoot:hunchentoot-warning)
  ((taskmaster :initarg :taskmaster :initform nil)
   (reason :initarg :reason :initform ""))
  (:report
   (lambda (condition stream)
     (with-slots (taskmaster reason) condition
       (format stream "Shutting-down taskmaster ~A was aborted by ~A."
               taskmaster reason))))
  (:documentation "Signalled when `hunchentoot:shutdown' for
  `recycling-taskmaster' gave up."))

(defmethod hunchentoot:shutdown ((taskmaster recycling-taskmaster))
  "Tell every threads to shutdown."
  ;; NOTE: I saw shutdown(2) can be usable for this purpose:
  ;;    https://stackoverflow.com/questions/9365282/c-linux-accept-blocking-after-socket-closed
  ;; However, usocket does not permit `usocket:socket-shotdown' to a listen socket.
  ;; Even when I `change-class'ed it, Allegro CL does not permit also.
  (loop
    initially
       (hunchentoot::with-lock-held ((hunchentoot::acceptor-shutdown-lock acceptor))
         (assert (hunchentoot::acceptor-shutdown-p acceptor)
                 () "acceptor-shutdown-p should be true"))
    
    with acceptor = (hunchentoot:taskmaster-acceptor taskmaster)
    with listen-socket = (hunchentoot::acceptor-listen-socket acceptor)
    with wake-try-count = 0

    ;; See the docstring of `recycling-taskmaster' for thread states and counters.
    for all-threads = (count-recycling-taskmaster-thread taskmaster)
    for busy-threads = (count-busy-thread taskmaster)
    as maybe-accepting-threads = (- all-threads busy-threads)
    
    while (plusp maybe-accepting-threads)
    do (or
        ;; Delete broken threads
        (delete-recycling-taskmaster-finished-thread taskmaster)
        ;; When the listen socket closed accidentally, gives up waking
        ;; threads.
        (unless (and listen-socket
                     (usocket:socket-state listen-socket))
          (hunchentoot::acceptor-log-message acceptor :warn "Wake-for-shutdown connect was gave up for closed socket: ~A"
                                             listen-socket)
          (warn 'recycling-taskmaster-shutdown-aborted-warning
                :taskmaster taskmaster :reason "the listen socket has been closed already.")
          (loop-finish))
        ;; Try to wake a thread waiting on the listen socket.
        ;; To avoid a permanent block, I use timeouts.
        (handler-case
            (wake-acceptor-for-shutdown-using-listen-socket listen-socket)
          (usocket:timeout-error (e)
            (hunchentoot::acceptor-log-message acceptor :info "Wake-for-shutdown connect failed by timeout: ~A" e)
            (when (> (incf wake-try-count) +wake-acceptor-for-shutdown-max-retry-count+)
              (warn 'recycling-taskmaster-shutdown-aborted-warning
                    :taskmaster taskmaster
                    :reason (format nil "Timeout occured more than ~R times." +wake-acceptor-for-shutdown-max-retry-count+) )
              (loop-finish)))
          (error (e)
            (hunchentoot::acceptor-log-message acceptor :error "Wake-for-shutdown connect failed: ~A" e)
            (warn 'recycling-taskmaster-shutdown-aborted-warning
                    :taskmaster taskmaster
                    :reason (format nil "Failed to connect itself: ~A" e) )
            (loop-finish))))
    finally
       ;; When (zerop maybe-accepting-threads) here, rest threads are
       ;; in `hunchentoot::handle-incoming-connection%', so they can
       ;; be wait using `hunchentoot::acceptor-shutdown-queue'.
       (return (zerop maybe-accepting-threads))))


;;; escape hatch

(define-condition recycling-taskmaster-corrupted-error (cl:simple-error)
  ((object :initarg :object)
   (broken-slots :initarg :broken-slots :initform nil))
  (:report (lambda (condition stream)
             (with-slots (object broken-slots) condition
               (format stream "Object ~A slots ~A is corrupted, probably by interrupts."
                       object broken-slots))))
  (:documentation "Signalled when the object was broken by interrupts
  of `abandon-taskmaster'"))

(defmethod abandon-taskmaster ((taskmaster recycling-taskmaster) &key (lock t))
  "Abandon all threads kept in TASKMASTER. This is a last resort
 for handling corrupted taskmaster objects."
  (let ((thread-list
          (flet ((steal-threads ()
                   (prog1 (hash-table-keys (hunchentoot::acceptor-process taskmaster))
                     (clrhash (hunchentoot::acceptor-process taskmaster)))))
            (if lock
                (hunchentoot::with-lock-held ((recycling-taskmaster-acceptor-process-lock taskmaster))
                  (steal-threads))
                (steal-threads)))))
    (dolist (thread thread-list)
      (typecase thread
        (bt2:thread
         (bt2:error-in-thread thread 'end-of-parallel-acceptor-thread))
        (otherwise                      ; bt1 thread object.
         (bt:interrupt-thread thread (lambda () (error 'end-of-parallel-acceptor-thread))))))
    (dolist (thread thread-list)
      (typecase thread
        (bt2:thread
         (bt2:join-thread thread))
        (otherwise
         (bt:join-thread thread))))
    ;; Checks corruption
    ;; TODO: Provide a restart.
    ;; FIXME: Threads waiting on locks or 'wait-queue' may be left.
    (let ((broken-slots nil))
      (unless (zerop (hash-table-count (hunchentoot::acceptor-process taskmaster)))
        (push 'hunchentoot::acceptor-process broken-slots))
      (unless (zerop (hunchentoot::taskmaster-thread-count taskmaster))
        (push 'hunchentoot::thread-count broken-slots))
      (unless (zerop (hunchentoot::taskmaster-accept-count taskmaster))
        (push 'hunchentoot::accept-count broken-slots))
      (when broken-slots
        (cerror "Ignore it."
                'recycling-taskmaster-corrupted-error
                :object taskmaster :broken-slots broken-slots)))
    taskmaster))
