(in-package #:hunchentoot-recycle)

(defclass parallel-acceptor (hunchentoot:acceptor)
  (
   ;; TODO

   ;; Use as-is
   ;; - port
   ;; - address
   ;; - name
   ;; - request-class
   ;; - reply-class
   ;; - taskmaster
   ;; - output-chunking-p
   ;; - input-chunking-p
   ;; - persistent-connections-p
   ;; - read-timeout
   ;; - write-timeout
   ;; - access-log-destination
   ;; - message-log-destination
   ;; - error-template-directory
   ;; - document-root
   
   ;; `hunchentoot:shutdown' related staffs must be same when the
   ;; default `hunchentoot:process-connection' is left as-is, because
   ;; it uses `with-acceptor-request-count-incremented'.
   ;; 
   ;; - requests-in-progress
   ;; - acceptor-shutdown-p
   ;; - shutdown-queue
   ;; - shutdown-lock
   )
  (:default-initargs
   :taskmaster 'recycling-taskmaster
   )
  )

;;; parallel-acceptor synchronizes accept(2) on the listen socket, so
;;; it requires the socket interface, usocket.  Sadly, this means
;;; Lispworks cannot be supported because usocket is unused.


;;; TODO

(defmethod hunchentoot:start ((acceptor parallel-acceptor))
  ;; currently, nothing special to do.
  (call-next-method))

(defmethod hunchentoot:stop ((acceptor parallel-acceptor) &key soft)
  ;; TODO: FIXME: Should parallel-acceptor avoid `wake-acceptor-for-shutdown'?
  (declare (ignorable soft))
  (call-next-method))

;;; `hunchentoot:started-p' is same.

(defmethod hunchentoot:start-listening ((acceptor parallel-acceptor))
  ;; currently, nothing special to do.
  (call-next-method))

(defmethod hunchentoot:accept-connections ((acceptor parallel-acceptor))
  "This works like the parental method, except removing some works for
sharing the listen socket."
  ;; `usocket:with-server-socket' is not used because it automatically
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
;;; TODO:
;;; - validate our own `accept-connections' is required not not.
;;;   I think points fixed above may not be strictly required.
;;; - By the way, above changes may be exported to the original
;;;   hunchentoot.

;;; `hunchentoot:initialize-connection-stream' is same.

;;; `hunchentoot:reset-connection-stream' is same.

;;; `hunchentoot:process-connection' is same.

;;; `hunchentoot:handle-request' is same.

;;; `hunchentoot:acceptor-dispatch-request' is same.

;;; `hunchentoot:acceptor-ssl-p' is same.


;;; `hunchentoot:detach-socket' is same.

(defmethod hunchentoot:acceptor-log-access ((acceptor parallel-acceptor) &key return-code)
  ;; FIXME: add a thread number?
  (declare (ignorable return-code))
  (call-next-method))

;;; `hunchentoot:acceptor-log-message' is same.

;;; `hunchentoot:acceptor-status-message' is same.

;;; `hunchentoot:acceptor-remove-session' is same.

(defmethod hunchentoot::acceptor-server-name ((acceptor parallel-acceptor))
  ;; FIXME: this is unexported!!
  ;; FIXME: a good version string..
  (format nil "Hunchentoot-recycle 0.0.0 (experimental), based on ~A"
          (call-next-method)))

;;; TODO
;; - Rework `hunchentoot:shutdown' related functions, utilizing shutdown(2).
