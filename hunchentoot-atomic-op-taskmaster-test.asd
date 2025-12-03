(defsystem #:hunchentoot-atomic-op-taskmaster-test
  :description "Tests for hunchentoot-atomic-op-taskmaster"
  :license "BSD 2-Clause"
  :author "Yokota Yuki"
  :depends-on (#:hunchentoot-atomic-op-taskmaster
               #:1am
               #:drakma)
  :pathname #.(make-pathname :directory '(:relative "test" "atomic-op-taskmaster-test"))
  :serial t
  :components ((:file "package")
               ;; TODO
               )
  :perform (prepare-op :before (o c)
                       (set (find-symbol* '#:*tests* '#:1am) '()))
  :perform (test-op (o s)
                    (symbol-call '#:1am '#:run)))
