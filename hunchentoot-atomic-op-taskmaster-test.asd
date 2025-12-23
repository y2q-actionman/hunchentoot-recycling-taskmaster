(defsystem #:hunchentoot-atomic-op-taskmaster-test
  :description "Tests for hunchentoot-atomic-op-taskmaster"
  :license "BSD 2-Clause"
  :author "Yokota Yuki"
  :version "0.0.1"
  :depends-on (#:hunchentoot-atomic-op-taskmaster
               #:1am
               #:hunchentoot-recycling-taskmaster-test)
  :pathname #.(make-pathname :directory '(:relative "test" "atomic-op-taskmaster-test"))
  :serial t
  :components ((:file "package")
               (:file "hunchentoot-tests"))
  :perform (prepare-op :before (o c)
                       (set (find-symbol* '#:*tests* '#:1am) '()))
  :perform (test-op (o s)
                    (symbol-call '#:1am '#:run)))
