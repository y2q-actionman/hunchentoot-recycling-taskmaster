(in-package :hunchentoot-recycling-taskmaster-benchmark)

;;; hunchentoot-recycling-taskmaster

(defparameter *hunchentoot-recycling-taskmaster-default-thread-count*
  hunchentoot-recycling-taskmaster::*default-standby-thread-count*)

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

(defun bench-hunchentoot-recycling-taskmaster
    (&optional (threads-list (list *hunchentoot-recycling-taskmaster-default-thread-count*)))
  (bench-hunchentoot-family-per-threads
   'hunchentoot-recycling-taskmaster:parallel-easy-acceptor
   'hunchentoot-recycling-taskmaster:recycling-taskmaster
   "hunchentoot-recycling-taskmaster"
   :hunchentoot-recycling-taskmaster
   :standby-thread-count threads-list
   *hunchentoot-recycling-taskmaster-default-thread-count*))


(defun bench-hunchentoot-recycling-taskmaster--extra ()
  (let ((*wrk-duration* 30)
        (*wrk-threads-and-connections* '((4 100)))
        (hunchentoot::*default-max-thread-count* nil)
        (hunchentoot::*default-max-accept-count* nil)
        (threads
          `(1 2 5
              ,*cl-tbnl-gserver-tmgr-default-thread-count* ; 8
              ,(- (nproc) (caar *cl-tbnl-gserver-tmgr--extra-test-wrk-threads*))
              ,(nproc)
              10 25 50 75 100
              125 150 175 200)))
    (bench-hunchentoot-recycling-taskmaster threads)))
