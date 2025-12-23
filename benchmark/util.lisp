(in-package :hunchentoot-recycling-taskmaster-benchmark)

(defun nproc ()
  (let ((nproc-str
          (with-output-to-string (*standard-output*)
            (uiop:run-program "nproc" :output t))))
    (parse-integer nproc-str)))

(defconstant +nproc+ (nproc))

(defparameter *wrk-duration* 10)
(defparameter *wrk-threads-and-connections*
  '((4 100) (4 10) (16 400))
  "derived from cl-tbnl-gserver-tmgr")

(defparameter *test-keep-alive* t)
(defparameter *test-no-keep-alive* t)

(defun generate-output-directory-path ()
  (multiple-value-bind (sec min hour day month year)
      (get-decoded-time)
    (let ((dir-name (format nil "~4,'0D-~2,'0D-~2,'0DT~2,'0D:~2,'0D:~2,'0D"
                            year month day hour min sec)))
      (make-pathname :directory (list :relative "benchmark" dir-name)))))

(defvar *output-directory* (generate-output-directory-path))

(defparameter *handler-sleep-seconds* 0.001) ; 1ms

(defvar *system-info-filename* "system-info.txt"
  "made by `prepare'")

(defun write-system-info (stream)
  (let ((system-symbols
          '(lisp-implementation-type
            lisp-implementation-version
            machine-type
            machine-version
            software-type
            software-version)))
    (loop for name in system-symbols
          as value = (funcall name)
          collect (list name value) into data
          maximize (length (symbol-name name)) into max-name-len 
          maximize (length value) into max-value-len
          finally
             (loop for (k v) in data
                   do (format stream "~vA	~vA~%"
                              max-name-len k max-value-len v)))
    (format stream "~&*handler-sleep-seconds*~C~F~%" #\tab *handler-sleep-seconds*)))

(defun run-wrk (host filename asdf-system-name &key (duration *wrk-duration*))
  (ensure-directories-exist *output-directory*)
  (with-open-file (*standard-output* (merge-pathnames filename *output-directory*) 
                                     :direction :output :if-exists :rename)
    (format t "~A~%" filename)
    (format t "~&~A~C~A~2%"
            asdf-system-name #\tab
            (asdf:component-version (asdf:find-system asdf-system-name)))
    (write-system-info *standard-output*)
    (terpri)
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
      (loop for (threads connections) in *wrk-threads-and-connections*
            if *test-keep-alive*
              do (run-tcd threads connections t)
            if *test-no-keep-alive*
              do (run-tcd threads connections nil)))))

(defun handler-small-sleep ()
  (when (plusp *handler-sleep-seconds*)
    (sleep *handler-sleep-seconds*)))

(defun wait-for-starting-server ()
  (sleep 1))
