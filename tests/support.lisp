(in-package #:maxwell/tests)

;;; Shared test helpers. Each tests/*-tests.lisp file defines a RUN-*-TESTS
;;; function that RUN-TESTS calls.

(defvar *failures* 0)

(defmacro check (form)
  `(unless ,form
     (incf *failures*)
     (format *error-output* "FAIL: ~S~%" ',form)))

(defun fake-response (&key content tool-calls)
  (maxwell/json:json-object
    "choices"
    (maxwell/json:json-array
      (maxwell/json:json-object
        "message"
        (let ((message (maxwell/json:json-object "role" "assistant"
                                                 "content" (or content maxwell/json:+null+))))
          (when tool-calls (setf (gethash "tool_calls" message) tool-calls))
          message)))))

(defun tool-call (code)
  (maxwell/json:json-object
    "id" "call-1"
    "type" "function"
    "function" (maxwell/json:json-object
                  "name" "common_lisp"
                  "arguments" (maxwell/json:encode
                                 (maxwell/json:json-object "code" code)))))
