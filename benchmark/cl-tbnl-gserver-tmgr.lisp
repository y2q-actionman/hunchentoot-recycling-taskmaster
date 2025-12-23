(in-package :hunchentoot-recycling-taskmaster-benchmark)

;;; cl-tbnl-gserver-tmgr

(defparameter *cl-tbnl-gserver-tmgr-default-thread-count*
  cl-tbnl-gserver-tmgr.tmgr::*gserver-tmgr-poolsize*)

(defun bench-cl-tbnl-gserver-tmgr (&optional (threads-list '(8)))
  (bench-hunchentoot-family-per-threads
   'hunchentoot:easy-acceptor
   'cl-tbnl-gserver-tmgr.tmgr:gserver-tmgr
   "cl-tbnl-gserver-tmgr"
   :cl-tbnl-gserver-tmgr
   :max-thread-count threads-list
   *cl-tbnl-gserver-tmgr-default-thread-count*))
