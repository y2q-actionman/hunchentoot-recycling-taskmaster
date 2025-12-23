(in-package :hunchentoot-recycling-taskmaster-benchmark)

(defun write-system-info-file (stream)
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
                              max-name-len k max-value-len v)))))

(defun prepare (&key (output-directory (generate-output-directory-path))
                  (system-info-filename *system-info-filename*))
  (setf *output-directory* output-directory)
  (ensure-directories-exist *output-directory*)
  (with-open-file (stream (merge-pathnames system-info-filename *output-directory*) 
                          :direction :output :if-exists :rename)
    (write-system-info-file stream)))

;;; for repl
(trace run-wrk)

(defun run ()
  (bench-hunchentoot)
  ;; (bench-hunchentoot-atomic-taskmaster)
  ;; (bench-hunchentoot-atomic-acceptor)
  (bench-hunchentoot-atomic-all)
  (bench-hunchentoot-recycling-taskmaster (list 8 *nproc*))
  ;; (bench-hunchentoot-recycling-taskmaster-atomic-all (list 8 *nproc*))
  ;; (bench-hunchentoot-recycling-taskmaster-atomic-acceptor (list 8 *nproc*))
  (bench-hunchentoot-recycling-taskmaster-atomic-taskmaster (list 8 *nproc*))
  (bench-cl-tbnl-gserver-tmgr (list 8 *nproc*))
  (bench-quux-hunchentoot)

  ;; (bench-house) ; broken

  (progn
    ;; (bench-conserv) ; I don't know how to use.
    (bench-wookie)
    (bench-woo (list nil 8 *nproc*))
    ;; (bench-woo-callback (list nil 8 *nproc*))
    ))
