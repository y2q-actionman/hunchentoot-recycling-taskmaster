(defsystem #:hunchentoot-recycle-test
  :description "Tests for hunchentoot-recycle"
  :license "BSD 2-Clause"
  :author "Yokota Yuki"
  :depends-on (#:hunchentoot-recycle
               #:1am
               #:drakma)
  :pathname #.(make-pathname :directory '(:relative "test"))
  :serial t
  :components ((:file "package")
               ;; TODO
               )
  :perform (prepare-op :before (o c)
                       (set (find-symbol* '#:*tests* '#:1am) '()))
  :perform (test-op (o s)
                    (symbol-call '#:1am '#:run)))
