(in-package #:hunchentoot-recycle)

(defclass recycling-taskmaster (hunchentoot:multi-threaded-taskmaster)
  ;; TODO: one-thread-per-connection-taskmaster slots.
  ())

(defmethod hunchentoot:execute-acceptor ((taskmaster recycling-taskmaster))
  "Make initial worker threads."
  (error "TODO: Under implementation"))

(defmethod hunchentoot:handle-incoming-connection ((taskmaster recycling-taskmaster) client-connection)
  "Nothing to do. Every workers should treat connections."
  (error "TODO: Under implementation"))

(defmethod hunchentoot:shutdown ((taskmaster recycling-taskmaster))
  "Saying every workers to shutdown."
  ;; TODO:
  ;; 1. Sets a flag saying "end itself" to the worker threads.
  ;; 2. wakes every threads waiting the listen socket (how?)
  ;;    https://stackoverflow.com/questions/9365282/c-linux-accept-blocking-after-socket-closed
  (error "TODO: Under implementation"))

(defmethod hunchentoot:create-request-handler-thread ((taskmaster recycling-taskmaster) client-connection)
  "Nothing to do. Every workers should treat connections."
  (error "TODO: Under implementation"))

(defmethod hunchentoot:too-many-taskmaster-requests ((taskmaster recycling-taskmaster) client-connection)
  "Nothing to do. Every workers should treat connections."
  (error "TODO: Under implementation"))

(defmethod hunchentoot:taskmaster-max-thread-count ((taskmaster recycling-taskmaster))
  ;; TODO: use atomics
  (error "TODO: Under implementation"))

(defmethod hunchentoot:taskmaster-max-accept-count ((taskmaster recycling-taskmaster))
  ;; TODO: use atomics
  (error "TODO: Under implementation"))

(defmethod hunchentoot:taskmaster-thread-count ((taskmaster recycling-taskmaster))
  ;; TODO: use atomics
  (error "TODO: Under implementation"))

(defmethod hunchentoot:increment-taskmaster-thread-count ((taskmaster recycling-taskmaster))
  ;; TODO: use atomics
  (error "TODO: Under implementation"))

(defmethod hunchentoot:decrement-taskmaster-thread-count ((taskmaster recycling-taskmaster))
  ;; TODO: use atomics
  (error "TODO: Under implementation"))

(defmethod hunchentoot:start-thread ((taskmaster recycling-taskmaster) thunk &key name)
  "Nothing to do, since workers of recycling-taskmaster has a
different model from `hunchentoot:multi-threaded-taskmaster'"
  (error "TODO: Under implementation"))
