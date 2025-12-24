(in-package #:hunchentoot-recycling-taskmaster)

;;; escape hatch

(define-condition recycling-taskmaster-corrupted-error (hunchentoot:hunchentoot-error)
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
  (request-making-thread taskmaster)    ; will stop thread-maker.
  (let ((thread-list
          (flet ((steal-threads ()
                   (prog1 (hash-table-keys (hunchentoot::acceptor-process taskmaster))
                     (clrhash (hunchentoot::acceptor-process taskmaster)))))
            (if lock
                (hunchentoot::with-lock-held ((recycling-taskmaster-acceptor-process-lock taskmaster))
                  (steal-threads))
                (steal-threads)))))
    (dolist (thread thread-list)
      ;; Uses `signal' instead of `error' because threads may be out
      ;; of `handler-case' which handles
      ;; `end-of-parallel-acceptor-thread'.
      (typecase thread
        (bt2:thread
         (bt2:signal-in-thread thread 'end-of-parallel-acceptor-thread))
        (otherwise                      ; bt1 thread object.
         (bt:interrupt-thread thread (lambda () (signal 'end-of-parallel-acceptor-thread))))))
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

(defmethod abandon-acceptor ((acceptor parallel-acceptor) &key (lock t))
  "To force an end of ACCEPTOR, destroy all threads and the listen
socket. If LOCK is nil, this is done *without* locking.  This function
should not be called in production (but required for my debugging)."
  (with-accessors ((shutdown-lock hunchentoot::acceptor-shutdown-lock)
                   (shutdown-p hunchentoot::acceptor-shutdown-p)
                   (listen-socket hunchentoot::acceptor-listen-socket))
      acceptor
    (if lock
        (hunchentoot::with-lock-held (shutdown-lock)
          (setf shutdown-p t))
        (setf shutdown-p t))
    (when listen-socket
      (ignore-errors
       (usocket:socket-close listen-socket))
      (setf listen-socket nil)))
  (abandon-taskmaster (hunchentoot::acceptor-taskmaster acceptor) :lock lock)
  ;; Checks corruption
  ;; TODO: Provide a restart.
  ;; FIXME: Threads waiting on locks or 'shutdown-queue' may be left.
  (unless (zerop (hunchentoot::acceptor-requests-in-progress acceptor))
    (cerror "Ignore it."
            'recycling-taskmaster-corrupted-error
            :object acceptor :broken-slots '(hunchentoot::requests-in-progress)))
  acceptor)
