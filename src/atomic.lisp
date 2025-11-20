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
