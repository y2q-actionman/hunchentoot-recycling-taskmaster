(defsystem #:hunchentoot-recycling-taskmaster-test
  :description "Tests for hunchentoot-recycling-taskmaster"
  :license "BSD 2-Clause"
  :author "Yokota Yuki"
  :depends-on (#:hunchentoot-recycling-taskmaster
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
