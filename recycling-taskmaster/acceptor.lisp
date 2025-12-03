(in-package #:hunchentoot-recycling-taskmaster)

(defclass parallel-acceptor (hunchentoot:acceptor)
  (
   ;; all slots are used as-is.
   ;; 
   ;; `hunchentoot:shutdown' related staffs must be same when the
   ;; default `hunchentoot:process-connection' is left as-is, because
   ;; it uses `with-acceptor-request-count-incremented'.
   )
  (:default-initargs
   :taskmaster (make-instance 'recycling-taskmaster)))

;;; parallel-acceptor synchronizes accept(2) on the listen socket, so
;;; it requires the socket interface, usocket.  Sadly, this means
;;; Lispworks cannot be supported because usocket is not used.

(defmethod hunchentoot:start :before ((acceptor parallel-acceptor))
  (let ((taskmaster (hunchentoot::acceptor-taskmaster acceptor)))
    (check-type taskmaster hunchentoot-recycling-taskmaster:recycling-taskmaster)))

(defmethod hunchentoot:stop ((acceptor parallel-acceptor) &key soft)
  "This works like the parental method, except some works for sharing
the listen socket."
  (with-lock-held ((hunchentoot::acceptor-shutdown-lock acceptor))
    (setf (hunchentoot::acceptor-shutdown-p acceptor) t))
  (let ((taskmaster (hunchentoot::acceptor-taskmaster acceptor)))
    (hunchentoot:shutdown taskmaster)
    (when soft
      ;; Wait for all thread end. This includes waiting for
      ;; `hunchentoot::acceptor-shutdown-queue' done by
      ;; original `hunchentoot:stop'.
      (wait-for-recycling-taskmaster-shutdown taskmaster)))
  (usocket:socket-close (hunchentoot::acceptor-listen-socket acceptor))
  (setf (hunchentoot::acceptor-listen-socket acceptor) nil)
  acceptor)

(defmethod hunchentoot:accept-connections ((acceptor parallel-acceptor))
  "This works like the parental method, except some works for sharing
the listen socket."
  ;; `usocket:with-server-socket' cannot be used because it automatically
  ;; calls close(2) but every parallel-acceptors should not do
  ;; it. close(2) should be called only once, by `hunchentoot:stop'.
  (let ((listener (hunchentoot::acceptor-listen-socket acceptor)))
    (usocket:with-mapped-conditions (listener)
      (loop
        (with-lock-held ((hunchentoot::acceptor-shutdown-lock acceptor))
          (when (hunchentoot::acceptor-shutdown-p acceptor)
            (return)))
        ;; I think `usocket:wait-for-input' is not required because it
        ;; works over only one listen socket and the socket blocks
        ;; `usocket:socket-accept' until accept(2) returns.
        (when-let (client-connection
                   (handler-case (usocket:socket-accept listener)
                     ;; ignore condition
                     (usocket:connection-aborted-error ())))
          (hunchentoot::set-timeouts client-connection
                                     (hunchentoot:acceptor-read-timeout acceptor)
                                     (hunchentoot:acceptor-write-timeout acceptor))
          (hunchentoot:handle-incoming-connection
           (hunchentoot::acceptor-taskmaster acceptor)
           client-connection))))))


(defmethod hunchentoot::acceptor-server-name ((acceptor parallel-acceptor))
  ;; FIXME: a good version string..
  (format nil "hunchentoot-recycling-taskmaster 0.0.0 (experimental), based on ~A"
          (call-next-method)))

(defmethod kill ((acceptor parallel-acceptor))
  "To force an end of ACCEPTOR, destroy all threads and the listen
 socket *without* locking.  This function should not be called in
 production (but required for my debugging)."
  (with-accessors ((shutdown-p hunchentoot::acceptor-shutdown-p)
                   (listen-socket hunchentoot::acceptor-listen-socket))
      acceptor
    (setf shutdown-p t)
    (when listen-socket
      (ignore-errors
       (usocket:socket-close listen-socket))
      (setf listen-socket nil)))
  (abandon-taskmaster (hunchentoot::acceptor-taskmaster acceptor)))

;;; Derived classes
(defclass parallel-ssl-acceptor (parallel-acceptor hunchentoot:ssl-acceptor)
  ())

(defclass parallel-easy-acceptor (parallel-acceptor hunchentoot:easy-acceptor)
  ())

(defclass parallel-easy-ssl-acceptor (parallel-easy-acceptor parallel-ssl-acceptor)
  ())

