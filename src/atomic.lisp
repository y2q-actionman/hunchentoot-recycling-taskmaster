(in-package #:hunchentoot-recycle)

(defclass atomic-taskmaster (hunchentoot:one-thread-per-connection-taskmaster)
  ((hunchentoot::accept-count
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
