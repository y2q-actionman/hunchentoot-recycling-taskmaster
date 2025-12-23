(in-package :hunchentoot-recycling-taskmaster-benchmark)

;;; quux-hunchentoot

(defun bench-quux-hunchentoot ()
  (bench-hunchentoot-using-class 'hunchentoot:easy-acceptor
                                 'quux-hunchentoot:thread-pooling-taskmaster
                                 "quux-hunchentoot.log"
                                 :quux-hunchentoot))
