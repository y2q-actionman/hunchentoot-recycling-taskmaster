(in-package #:hunchentoot-recycle)

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

(defmethod hunchentoot:stop ((acceptor parallel-acceptor) &key soft)
  "This works like the parental method, except some works for sharing
the listen socket."
  (hunchentoot::with-lock-held ((hunchentoot::acceptor-shutdown-lock acceptor))
    (setf (hunchentoot::acceptor-shutdown-p acceptor) t))
  (let ((taskmaster (hunchentoot::acceptor-taskmaster acceptor)))
    (hunchentoot:shutdown taskmaster)
    (when soft
      ;; Wait for all worker ends. This includes waiting for
      ;; `hunchentoot::acceptor-shutdown-queue' done by
      ;; original `hunchentoot:stop'.
      (hunchentoot::with-lock-held ((recycling-taskmaster-acceptor-process-lock taskmaster))
        (when (plusp (count-recycling-taskmaster-thread taskmaster :lock nil))
          (wait-for-recycling-taskmaster-shutdown taskmaster)))))
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
        (hunchentoot::with-lock-held ((hunchentoot::acceptor-shutdown-lock acceptor))
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
  (format nil "Hunchentoot-recycle 0.0.0 (experimental), based on ~A"
          (call-next-method)))


;;; Derived classes
(defclass parallel-ssl-acceptor (parallel-acceptor hunchentoot:ssl-acceptor)
  ())

(defclass parallel-easy-acceptor (parallel-acceptor hunchentoot:easy-acceptor)
  ())

(defclass parallel-easy-ssl-acceptor (parallel-easy-acceptor parallel-ssl-acceptor)
  ())

