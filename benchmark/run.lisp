(in-package :hunchentoot-recycling-taskmaster-benchmark)

(defun prepare (&key (output-directory (generate-output-directory-path))
                  (system-info-filename *system-info-filename*))
  (setf *output-directory* output-directory)
  (ensure-directories-exist *output-directory*)
  (with-open-file (stream (merge-pathnames system-info-filename *output-directory*) 
                          :direction :output :if-exists :rename)
    (write-system-info stream)))

;;; for repl
(trace run-wrk)

(defun run ()
  (handler-bind ((error (lambda (c)
                          (warn "caught conditon ~A. Now trying to cl:continue" c)
                          (continue))))
    (bench-hunchentoot)
    ;; (bench-hunchentoot-atomic-taskmaster)
    ;; (bench-hunchentoot-atomic-acceptor)
    (bench-hunchentoot-atomic-all)
    (bench-hunchentoot-recycling-taskmaster (list 8 +nproc+))
    ;; (bench-hunchentoot-recycling-taskmaster-atomic-all (list 8 +nproc+))
    ;; (bench-hunchentoot-recycling-taskmaster-atomic-acceptor (list 8 +nproc+))
    (bench-hunchentoot-recycling-taskmaster-atomic-taskmaster (list 8 +nproc+))
    (bench-cl-tbnl-gserver-tmgr (list 8 +nproc+))
    (bench-quux-hunchentoot)

    ;; (bench-house) ; broken

    #+sbcl
    (progn
      ;; (bench-conserv) ; I don't know how to use.
      (bench-wookie)
      (bench-woo (list nil 8 +nproc+))
      ;; (bench-woo-callback (list nil 8 +nproc+))
      )))
