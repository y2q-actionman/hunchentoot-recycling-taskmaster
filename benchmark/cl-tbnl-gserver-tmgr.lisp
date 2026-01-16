(in-package :hunchentoot-recycling-taskmaster-benchmark)

;;; cl-tbnl-gserver-tmgr

(defparameter *cl-tbnl-gserver-tmgr-default-thread-count*
  cl-tbnl-gserver-tmgr.tmgr::*gserver-tmgr-poolsize*)

(defun bench-cl-tbnl-gserver-tmgr
    (&optional (threads-list (list *cl-tbnl-gserver-tmgr-default-thread-count*)))
  (log:config :fatal)
  (cond
    ((find-package :sento)
     (bench-hunchentoot-family-per-threads
      'hunchentoot:easy-acceptor
      'cl-tbnl-gserver-tmgr.tmgr:gserver-tmgr
      "cl-tbnl-gserver-tmgr"
      :cl-tbnl-gserver-tmgr
      :max-thread-count threads-list
      *cl-tbnl-gserver-tmgr-default-thread-count*))
    ((find-package :cl-gserver)
     ;; See `bench-hunchentoot-family-per-threads'
     (loop
       with thread-var = (find-symbol (string '#:*gserver-tmgr-poolsize*)
                                      '#:cl-tbnl-gserver-tmgr.tmgr)
       with logname-prefix = "cl-tbnl-gserver-tmgr--2020-06-29"
       with threads-default-count = *cl-tbnl-gserver-tmgr-default-thread-count*
       for threads in threads-list
       as logname = (format nil "~A_threads-~A~@[-default~*~].log"
                            logname-prefix threads
                            (eql threads threads-default-count))
       do (setf (symbol-value thread-var) threads)
       collect
       (progv (list thread-var) (list threads)
         (bench-hunchentoot-using-class
          'hunchentoot:easy-acceptor
          'cl-tbnl-gserver-tmgr.tmgr:gserver-tmgr
          logname 
          :cl-tbnl-gserver-tmgr))
       finally
          (setf (symbol-value thread-var) threads-default-count)))))


(defun bench-cl-tbnl-gserver-tmgr--extra ()
  (let ((*wrk-duration* 30)
        (*wrk-threads-and-connections* '((4 100)))
        (hunchentoot::*default-max-thread-count* nil)
        (hunchentoot::*default-max-accept-count* nil)
        (threads
          `(1 2 5
              ,*cl-tbnl-gserver-tmgr-default-thread-count* ; 8
              ,@ (let ((diff (- (nproc) (caar *wrk-threads-and-connections*))))
                   (if (plusp diff) `(,diff)))
              ,(nproc)
              10 25 50 75 100
              125 150 175 200)))
    (bench-cl-tbnl-gserver-tmgr threads)))


(defun bench-cl-tbnl-gserver-tmgr--extra-in-my-cpu-cores ()
  (let ((*wrk-duration* 30)
        (*wrk-threads-and-connections* '((4 4)))
        (hunchentoot::*default-max-thread-count* nil)
        (hunchentoot::*default-max-accept-count* nil)
        (threads '(1 2 3 4 5 6 7 8 9)))
    (bench-cl-tbnl-gserver-tmgr threads)))
