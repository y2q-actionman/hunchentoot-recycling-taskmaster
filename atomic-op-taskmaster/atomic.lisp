(in-package #:hunchentoot-atomic-op-taskmaster)

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
    "Works same as the original one except it is an atomic-integer."))
  (:documentation "An acceptor works just like
`hunchentoot:acceptor' except using atomic integers."))

(defmethod hunchentoot::acceptor-requests-in-progress ((acceptor atomic-acceptor))
  (bt2:atomic-integer-value (atomic-acceptor-requests-in-progress-cell acceptor)))

(defgeneric hunchentoot::do-with-acceptor-request-count-incremented (acceptor function))

(in-package #:hunchentoot)

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

(in-package #:hunchentoot-atomic-op-taskmaster)

(defmethod hunchentoot::do-with-acceptor-request-count-incremented ((hunchentoot::*acceptor* atomic-acceptor) function)
  (bt2:atomic-integer-incf (atomic-acceptor-requests-in-progress-cell hunchentoot::*acceptor*))
  (unwind-protect
       (funcall function)
    (bt2:atomic-integer-decf (atomic-acceptor-requests-in-progress-cell hunchentoot::*acceptor*))
    (hunchentoot::with-lock-held ((hunchentoot::acceptor-shutdown-lock hunchentoot::*acceptor*))
      (when (hunchentoot::acceptor-shutdown-p hunchentoot::*acceptor*)
        (hunchentoot::condition-variable-signal (hunchentoot::acceptor-shutdown-queue hunchentoot::*acceptor*))))))

;;; Derived classes
(defclass atomic-ssl-acceptor (atomic-acceptor hunchentoot:ssl-acceptor)
  ())

(defclass atomic-easy-acceptor (atomic-acceptor hunchentoot:easy-acceptor)
  ())

(defclass atomic-easy-ssl-acceptor (atomic-easy-acceptor atomic-ssl-acceptor)
  ())
