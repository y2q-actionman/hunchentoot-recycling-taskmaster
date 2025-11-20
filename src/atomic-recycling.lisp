(in-package #:hunchentoot-recycle)

(defclass atomic-recycling-taskmaster (recycling-taskmaster atomic-taskmaster)
  ())

(defclass atomic-parallel-acceptor (parallel-acceptor atomic-acceptor)
  ()
  (:default-initargs
   :taskmaster (make-instance 'atomic-recycling-taskmaster)))

;;; Derived classes
(defclass atomic-parallel-ssl-acceptor (atomic-parallel-acceptor hunchentoot:ssl-acceptor)
  ())

(defclass atomic-parallel-easy-acceptor (atomic-parallel-acceptor hunchentoot:easy-acceptor)
  ())

(defclass atomic-parallel-easy-ssl-acceptor (atomic-parallel-easy-acceptor parallel-ssl-acceptor)
  ())
