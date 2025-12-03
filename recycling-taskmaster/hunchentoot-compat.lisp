(in-package #:hunchentoot-recycling-taskmaster)

(defmacro defun-alias-to-hunchentoot-compat (name lambda-list)
  "Defines a simple alias of a function in Hunchentoot."
  (let* ((hunchentoot-symbol (find-symbol (symbol-name name) '#:hunchentoot))
         (docstring (format nil "Just an alias of `~A:~A'" '#:hunchentoot hunchentoot-symbol)))
    `(progn
       (declaim (inline ,name))
       (defun ,name ,lambda-list
         ,docstring
         (,hunchentoot-symbol
          ,@ (if (eq (first lambda-list) '&key)
                 (loop for sym in (rest lambda-list)
                       collect (find-symbol (symbol-name sym) '#:keyword)
                       collect sym)
                 lambda-list))))))

(defun-alias-to-hunchentoot-compat make-lock (name))

(defun-alias-to-hunchentoot-compat make-condition-variable (&key name))

(defun-alias-to-hunchentoot-compat condition-variable-signal (condition-variable))

(defun-alias-to-hunchentoot-compat condition-variable-wait (condition-variable lock))

(defmacro with-lock-held ((lock) &body body)
  "Just an alias of `hunchentoot::with-lock-held'"
  `(hunchentoot::with-lock-held (,lock) ,@body))
