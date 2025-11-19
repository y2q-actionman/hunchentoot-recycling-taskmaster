(defsystem #:hunchentoot-recycle
  :description "An experiment to improve multithreading performance of hunchentoot without any additional dependencies."
  :license "BSD 2-Clause"               ; follows hunchentoot.
  :author "Yokota Yuki"
  :depends-on (#:hunchentoot)
  :pathname #.(make-pathname :directory '(:relative "src"))
  :serial t
  :components ((:file "package")
               (:file "taskmaster")
               (:file "acceptor"))
  :in-order-to ((test-op (test-op #:hunchentoot-recycle-test))))
