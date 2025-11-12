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
  (error "TODO: under implementation"))

(defmethod hunchentoot:stop ((acceptor parallel-acceptor) &key soft)
  (error "TODO: under implementation"))

;;; `hunchentoot:started-p' is same.

(defmethod hunchentoot:start-listening ((acceptor parallel-acceptor))
  (error "TODO: under implementation"))

(defmethod hunchentoot:accept-connections ((acceptor parallel-acceptor))
  (error "TODO: under implementation"))

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

(defmethod hunchentoot:acceptor-server-name ((acceptor parallel-acceptor))
  ;; FIXME: a good version string..
  (format nil "Hunchentoot-recycle 0.0.0 (experimental), based on ~A"
          (call-next-method)))
