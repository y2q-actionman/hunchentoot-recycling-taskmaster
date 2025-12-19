(in-package #:hunchentoot-atomic-op-taskmaster-test)

;;; Hunchentoot + atomic

(1am:test normal-tm-normal-ac
  (1am:is (run-hunchentoot-tests
           :taskmaster-class 'hunchentoot:one-thread-per-connection-taskmaster
           :acceptor-class 'hunchentoot:easy-acceptor)))

(1am:test atomic-tm-normal-ac
  (1am:is (run-hunchentoot-tests
           :taskmaster-class 'hunchentoot-atomic-op-taskmaster:atomic-taskmaster
           :acceptor-class 'hunchentoot:easy-acceptor)))

(1am:test normal-tm-atomic-ac
  (1am:is (run-hunchentoot-tests
           :taskmaster-class 'hunchentoot:one-thread-per-connection-taskmaster
           :acceptor-class 'hunchentoot-atomic-op-taskmaster:atomic-easy-acceptor)))

(1am:test atomic-tm-atomic-ac
  (1am:is (run-hunchentoot-tests
           :taskmaster-class 'hunchentoot-atomic-op-taskmaster:atomic-taskmaster
           :acceptor-class 'hunchentoot-atomic-op-taskmaster:atomic-easy-acceptor)))

;;; hunchentoot-recycling-taskmaster + atomic

(1am:test normal-rtm-normal-pac
  (1am:is (run-hunchentoot-tests
           :taskmaster-class 'hunchentoot-recycling-taskmaster:recycling-taskmaster
           :acceptor-class 'hunchentoot-recycling-taskmaster:parallel-easy-acceptor)))

(1am:test atomic-rtm-normal-pac
  (1am:is (run-hunchentoot-tests
           :taskmaster-class 'hunchentoot-atomic-op-taskmaster:atomic-recycling-taskmaster
           :acceptor-class 'hunchentoot-recycling-taskmaster:parallel-easy-acceptor)))

(1am:test normal-rtm-atomic-pac
  (1am:is (run-hunchentoot-tests
           :taskmaster-class 'hunchentoot-recycling-taskmaster:recycling-taskmaster
           :acceptor-class 'hunchentoot-atomic-op-taskmaster:atomic-parallel-easy-acceptor)))

(1am:test atomic-rtm-atomic-pac
  (1am:is (run-hunchentoot-tests
           :taskmaster-class 'hunchentoot-atomic-op-taskmaster:atomic-recycling-taskmaster
           :acceptor-class 'hunchentoot-atomic-op-taskmaster:atomic-parallel-easy-acceptor)))

;;; splited-thread-count

(1am:test splited-thread-count-rtm-normal-pac
  (1am:is (run-hunchentoot-tests
           :taskmaster-class 'hunchentoot-atomic-op-taskmaster:splited-thread-count-recycling-taskmaster
           :acceptor-class 'hunchentoot-recycling-taskmaster:parallel-easy-acceptor)))

(1am:test atomic-splited-thread-count-rtm-normal-pac
  (1am:is (run-hunchentoot-tests
           :taskmaster-class 'hunchentoot-atomic-op-taskmaster:atomic-splited-thread-count-recycling-taskmaster
           :acceptor-class 'hunchentoot-recycling-taskmaster:parallel-easy-acceptor)))

(1am:test atomic-splited-thread-count-rtm-atomic-pac
  (1am:is (run-hunchentoot-tests
           :taskmaster-class 'hunchentoot-atomic-op-taskmaster:atomic-splited-thread-count-recycling-taskmaster
           :acceptor-class 'hunchentoot-atomic-op-taskmaster:atomic-parallel-easy-acceptor)))
