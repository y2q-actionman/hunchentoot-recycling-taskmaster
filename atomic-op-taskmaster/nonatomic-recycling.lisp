(in-package #:hunchentoot-atomic-op-taskmaster)

;;; only for benchmarking

(defclass nonatomic-recycling-taskmaster (recycling-taskmaster)
  ((busy-thread-count
    :initform 0))
  (:documentation "I found only 'busy-thread-count' slot
contributes the benchmark score. This class is only for
testing it."))

(defmethod increment-recycling-taskmaster-busy-thread-count ((taskmaster nonatomic-recycling-taskmaster))
  (hunchentoot::with-lock-held ((recycling-taskmaster-busy-thread-count-lock taskmaster))
    (incf (recycling-taskmaster-busy-thread-count taskmaster))))

(defmethod decrement-recycling-taskmaster-busy-thread-count ((taskmaster nonatomic-recycling-taskmaster))
  (hunchentoot::with-lock-held ((recycling-taskmaster-busy-thread-count-lock taskmaster))
    (let ((rest (decf (recycling-taskmaster-busy-thread-count taskmaster))))
      (when (<= rest 0)
        (hunchentoot::condition-variable-signal (recycling-taskmaster-busy-thread-count-queue taskmaster)))
      rest)))

(defmethod wait-end-of-handle-incoming-connection ((taskmaster nonatomic-recycling-taskmaster))
  "Waits threads in `hunchentoot:handle-incoming-connection' end."
  (hunchentoot::with-lock-held ((recycling-taskmaster-busy-thread-count-lock taskmaster))
    (when (plusp (recycling-taskmaster-busy-thread-count taskmaster))
      (hunchentoot::condition-variable-wait
       (recycling-taskmaster-busy-thread-count-queue taskmaster)
       (recycling-taskmaster-busy-thread-count-lock taskmaster)))))
