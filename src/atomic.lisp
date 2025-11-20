(in-package #:hunchentoot-recycle)

(defclass atomic-taskmaster (hunchentoot:one-thread-per-connection-taskmaster)
  ((hunchentoot::thread-count
    :type bt2:atomic-integer
    :initform (bt2:make-atomic-integer)
    :accessor atomic-taskmaster-thread-count-cell
    :documentation
    "Works same as the original one except it is an atomic-integer.")
   (hunchentoot::accept-count
    :type bt2:atomic-integer
    :initform (bt2:make-atomic-integer)
    :accessor atomic-taskmaster-accept-count-cell
    :documentation
    "Works same as the original one except it is an atomic-integer."))
  (:documentation "A taskmaster works just like
`hunchentoot:one-thread-per-connection-taskmaster' except using
atomic integers."))

(defmethod hunchentoot::taskmaster-accept-count ((taskmaster atomic-taskmaster))
  (bt2:atomic-integer-value (atomic-taskmaster-accept-count-cell taskmaster)))

(defmethod hunchentoot::increment-taskmaster-accept-count ((taskmaster atomic-taskmaster))
  (when (hunchentoot::taskmaster-max-accept-count taskmaster)
    (bt2:atomic-integer-incf (atomic-taskmaster-accept-count-cell taskmaster))))

(defmethod hunchentoot::decrement-taskmaster-accept-count ((taskmaster atomic-taskmaster))
  (when (hunchentoot::taskmaster-max-accept-count taskmaster)
    (bt2:atomic-integer-decf (atomic-taskmaster-accept-count-cell taskmaster))))

(defmethod hunchentoot::taskmaster-thread-count ((taskmaster atomic-taskmaster))
  (bt2:atomic-integer-value (atomic-taskmaster-thread-count-cell taskmaster)))

(defmethod hunchentoot::increment-taskmaster-thread-count ((taskmaster atomic-taskmaster))
  (when (hunchentoot::taskmaster-max-thread-count taskmaster)
    (bt2:atomic-integer-incf (atomic-taskmaster-thread-count-cell taskmaster))))

(defmethod hunchentoot::decrement-taskmaster-thread-count ((taskmaster atomic-taskmaster))
  (when (hunchentoot::taskmaster-max-thread-count taskmaster)
    (prog2
        (bt2:atomic-integer-decf (atomic-taskmaster-thread-count-cell taskmaster))
        (hunchentoot::decrement-taskmaster-accept-count taskmaster)
      (when (and (hunchentoot::taskmaster-max-accept-count taskmaster)
                 (< (hunchentoot::taskmaster-thread-count taskmaster)
                    (hunchentoot::taskmaster-max-accept-count taskmaster)))
        (hunchentoot::note-free-connection taskmaster)))))


(defclass atomic-acceptor (hunchentoot:acceptor)
  ((hunchentoot::requests-in-progress
    :type bt2:atomic-integer
    :initform (bt2:make-atomic-integer)
    :accessor atomic-acceptor-requests-in-progress-cell
    :documentation
    "Works same as the original one except it is an atomic-integer.")
   (hunchentoot::acceptor-shutdown-p
    :type bt2:atomic-integer
    :initform (bt2:make-atomic-integer :value 1)
    :accessor atomic-acceptor-shutdown-p-cell
    :documentation "Works same as the original one except it is an atomic-integer. 1 means true, 0 means false."))
  (:documentation "An acceptor works just like
`hunchentoot:acceptor' except using atomic integers."))

(defmethod hunchentoot::acceptor-requests-in-progress ((acceptor atomic-acceptor))
  (bt2:atomic-integer-value (atomic-acceptor-requests-in-progress-cell acceptor)))

(defmethod hunchentoot::acceptor-shutdown-p ((acceptor atomic-acceptor))
  (= 1 (bt2:atomic-integer-value (atomic-acceptor-shutdown-p-cell acceptor))))

(defmethod (setf hunchentoot::acceptor-shutdown-p) (boolean (acceptor atomic-acceptor))
  (setf (bt2:atomic-integer-value (atomic-acceptor-shutdown-p-cell acceptor))
        (if boolean 1 0)))

(defmethod hunchentoot:stop ((acceptor atomic-acceptor) &key soft)
  (setf (hunchentoot::acceptor-shutdown-p acceptor) t)
  #-lispworks
  (hunchentoot::wake-acceptor-for-shutdown acceptor)
  (when soft
    (when (plusp (hunchentoot::acceptor-requests-in-progress acceptor))
      (hunchentoot::with-lock-held ((hunchentoot::acceptor-shutdown-lock acceptor))
        (when (plusp (hunchentoot::acceptor-requests-in-progress acceptor))
          (hunchentoot::condition-variable-wait (hunchentoot::acceptor-shutdown-queue acceptor)
                                                (hunchentoot::acceptor-shutdown-lock acceptor))))))
  (hunchentoot:shutdown (hunchentoot::acceptor-taskmaster acceptor))
  #-lispworks
  (usocket:socket-close (hunchentoot::acceptor-listen-socket acceptor))
  #-lispworks
  (setf (hunchentoot::acceptor-listen-socket acceptor) nil)
  #+lispworks
  (mp:process-kill (hunchentoot::acceptor-process acceptor))
  acceptor)

(defgeneric hunchentoot::do-with-acceptor-request-count-incremented (acceptor function))

(in-package :hunchentoot)

;;; same as the original 
(defmethod do-with-acceptor-request-count-incremented (*acceptor* function)
  (with-lock-held ((acceptor-shutdown-lock *acceptor*))
    (incf (acceptor-requests-in-progress *acceptor*)))
  (unwind-protect
       (funcall function)
    (with-lock-held ((acceptor-shutdown-lock *acceptor*))
      (decf (acceptor-requests-in-progress *acceptor*))
      (when (acceptor-shutdown-p *acceptor*)
        (condition-variable-signal (acceptor-shutdown-queue *acceptor*))))))

(in-package :hunchentoot-recycle)

(defmethod hunchentoot::do-with-acceptor-request-count-incremented ((hunchentoot::*acceptor* atomic-acceptor) function)
  (bt2:atomic-integer-incf (atomic-acceptor-requests-in-progress-cell hunchentoot::*acceptor*))
  (unwind-protect
       (funcall function)
    (bt2:atomic-integer-decf (atomic-acceptor-requests-in-progress-cell hunchentoot::*acceptor*))
    (when (hunchentoot::acceptor-shutdown-p hunchentoot::*acceptor*)
      (hunchentoot::with-lock-held ((hunchentoot::acceptor-shutdown-lock hunchentoot::*acceptor*))
        (when (hunchentoot::acceptor-shutdown-p hunchentoot::*acceptor*)
          (hunchentoot::condition-variable-signal (hunchentoot::acceptor-shutdown-queue hunchentoot::*acceptor*)))))))

(defmethod hunchentoot::accept-connections ((acceptor atomic-acceptor))
  (usocket:with-server-socket (listener (hunchentoot::acceptor-listen-socket acceptor))
    (loop
      (when (hunchentoot::acceptor-shutdown-p acceptor)
        (return))
      (when (usocket:wait-for-input listener :ready-only t)
        (alexandria:when-let (client-connection
                              (handler-case (usocket:socket-accept listener)
                                ;; ignore condition
                                (usocket:connection-aborted-error ())))
          (hunchentoot::set-timeouts client-connection
                                     (hunchentoot::acceptor-read-timeout acceptor)
                                     (hunchentoot::acceptor-write-timeout acceptor))
          (hunchentoot::handle-incoming-connection (hunchentoot::acceptor-taskmaster acceptor)
                                                   client-connection))))))

;;; Derived classes
(defclass atomic-ssl-acceptor (atomic-acceptor hunchentoot:ssl-acceptor)
  ())

(defclass atomic-easy-acceptor (atomic-acceptor hunchentoot:easy-acceptor)
  ())

(defclass atomic-easy-ssl-acceptor (atomic-easy-acceptor atomic-ssl-acceptor)
  ())
