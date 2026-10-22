(defpackage #:maxwell/tests
  (:use #:cl)
  ;; The thread tests reach Bordeaux Threads directly, as Maxwell does.
  (:local-nicknames (#:bt #:bordeaux-threads))
  (:export #:run-tests))

(in-package #:maxwell/tests)
