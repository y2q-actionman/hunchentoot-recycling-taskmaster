(in-package :hunchentoot-recycling-taskmaster-benchmark)

;;; hunchentoot-recycling-taskmaster

(defparameter *hunchentoot-recycling-taskmaster-default-thread-count*
  hunchentoot-recycling-taskmaster::*default-initial-thread-count*)

(defun bench-hunchentoot-family-per-threads (acceptor-class taskmaster-class logname-prefix
                                             asdf-system-name
                                             taskmaster-thread-argname threads-list
                                             threads-default-count)
  (loop
    for threads in threads-list
    as logname = (format nil "~A_threads-~A~@[-default~*~].log"
                         logname-prefix threads
                         (eql threads threads-default-count))
    collect
    (bench-hunchentoot-using-class acceptor-class taskmaster-class logname asdf-system-name
                                   :taskmaster-args (list taskmaster-thread-argname threads))))

(defun bench-hunchentoot-recycling-taskmaster (&optional (threads-list '(8)))
  (bench-hunchentoot-family-per-threads
   'hunchentoot-recycling-taskmaster:parallel-easy-acceptor
   'hunchentoot-recycling-taskmaster:recycling-taskmaster
   "hunchentoot-recycling-taskmaster"
   :hunchentoot-recycling-taskmaster
   :initial-thread-count threads-list
   *hunchentoot-recycling-taskmaster-default-thread-count*))
