(in-package :cl-user)

(ql:quickload "hunchentoot")
(ql:quickload "cl-tbnl-gserver-tmgr")
(ql:quickload "hunchentoot-recycle")
(ql:quickload "quux-hunchentoot")

;;;

(defparameter *wrk-duration* 10)

(defun run-wrk (host filename)
  (with-open-file (*standard-output* filename :direction :output :if-exists :rename)
    (format t "~A ~A ~A~2%"
            filename  (lisp-implementation-type) (lisp-implementation-version))
    (finish-output)
    (flet ((run-tcd (thread connection &optional (duration *wrk-duration*))
             (uiop:run-program (list "wrk"
                                     "-t" (princ-to-string thread)
                                     "-c" (princ-to-string connection)
                                     "-d" (princ-to-string duration)
                                     host)
                               :output t)
             (finish-output)))
      (run-tcd 4 100)
      (terpri) (finish-output)
      (run-tcd 4 10)
      (terpri) (finish-output)
      (run-tcd 16 400)
      (terpri) (finish-output))))

;;; Hunchentoot

(hunchentoot:define-easy-handler (say-yo :uri "/yo") (name)
  (setf (hunchentoot:content-type*) "text/plain")
  (format nil "Hey~@[ ~A~]!" name))

(defun bench-hunchentoot ()
  (let ((server (make-instance 'hunchentoot:easy-acceptor
                               :message-log-destination nil
                               :access-log-destination nil
                               :port 4242)))
    (hunchentoot:start server)
    (unwind-protect
         (run-wrk "http://localhost:4242/yo" "hunchentoot_default.log")
      (hunchentoot:stop server :soft t))
    server))

;;; cl-tbnl-gserver-tmgr

(defparameter *cl-tbnl-gserver-tmgr-default-thread-count*
  cl-tbnl-gserver-tmgr.tmgr::*gserver-tmgr-poolsize*)

(defun bench-cl-tbnl-gserver-tmgr ()
  (loop
    for threads in '(4 8 16)
    as logname = (format nil "cl-tbnl-gserver-tmgr_threads-~D~@[-default~*~].log"
                         threads (eql threads *cl-tbnl-gserver-tmgr-default-thread-count*))
    as server = (make-instance 'hunchentoot:easy-acceptor
                               :message-log-destination nil
                               :access-log-destination nil
                               :port 4242
                               :taskmaster (make-instance 'cl-tbnl-gserver-tmgr.tmgr:gserver-tmgr
                                                          :max-thread-count threads))
    collect server
    do (hunchentoot:start server)
       (unwind-protect
            (run-wrk "http://localhost:4242/yo" logname)
         (hunchentoot:stop server :soft t))))

;;; hunchentoot-recycle

(defparameter *hunchentoot-recycle-default-thread-count*
  hunchentoot-recycle::*default-initial-thread-count*)

(defun bench-hunchentoot-recycle ()
  (loop
    for threads in '(4 8 16)
    as logname = (format nil "hunchentoot-recycle_threads-~A~@[-default~*~].log"
                         threads
                         (eql threads *cl-tbnl-gserver-tmgr-default-thread-count*))
    as taskmaster = (make-instance 'hunchentoot-recycle:recycling-taskmaster
                                   :initial-thread-count threads)
    as server = (make-instance 'hunchentoot-recycle:parallel-easy-acceptor
                               :message-log-destination nil
                               :access-log-destination nil
                               :port 4242
                               :taskmaster taskmaster)
    collect server
    do (hunchentoot:start server)
       (unwind-protect
            (run-wrk "http://localhost:4242/yo" logname)
         (hunchentoot:stop server :soft t))))


;;; for repl
(trace run-wrk)
(defun bench-all ()
  (bench-hunchentoot)
  (bench-cl-tbnl-gserver-tmgr)
  (bench-hunchentoot-recycle))
