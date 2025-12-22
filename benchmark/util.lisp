(in-package :hunchentoot-recycling-taskmaster-benchmark)

(defun nproc ()
  (let ((nproc-str
          (with-output-to-string (*standard-output*)
            (uiop:run-program "nproc" :output t))))
    (parse-integer nproc-str)))

(defparameter *nproc* (nproc))

(defparameter *wrk-duration* 10)
(defparameter *test-keep-alive* t)
(defparameter *test-no-keep-alive* t)

(defun run-wrk (host filename &key (duration *wrk-duration*))
  (with-open-file (*standard-output* filename :direction :output :if-exists :rename)
    (format t "~A ~A ~A~2%"
            filename  (lisp-implementation-type) (lisp-implementation-version))
    (finish-output)
    (flet ((run-tcd (thread connection keep-alive)
             (let ((options (list "-t" (princ-to-string thread)
                                  "-c" (princ-to-string connection)
                                  "-d" (princ-to-string duration)
                                  host)))
               (unless keep-alive
                 (push "Connection: close" options)
                 (push "-H" options))
               (format t "# ~{~A ~^~}~%" options)
               (finish-output)
               (uiop:run-program (list* "wrk" options) :output t))
             (terpri)
             (finish-output)))
      (loop for (threads connections) in '((4 100) (4 10) (16 400))
            if *test-keep-alive*
              do (run-tcd threads connections t)
            if *test-no-keep-alive*
              do (run-tcd threads connections nil)))))
