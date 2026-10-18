(in-package #:maxwell)

;;; Provider requests run in a child process. At the interactive prompt that
;;; child would otherwise block Maxwell's only thread, so it is started
;;; asynchronously and the prompt keeps servicing keystrokes while it runs.
;;; Both hooks are NIL everywhere else, which leaves one-shot, scripted, and
;;; test runs on the plain blocking path.

(defvar *input-poll* nil
  "When non-NIL, a function called repeatedly while a subprocess runs, so that
the interactive prompt can keep handling keystrokes.")

(defvar *input-hide* nil
  "When non-NIL, a function called once when a subprocess finishes, so that the
interactive prompt can erase its input line before other output appears.")

(defun write-encoded-file (path text)
  (with-open-file (stream path :direction :output :if-exists :supersede
                               :if-does-not-exist :create
                               :external-format :utf-8)
    (write-string text stream))
  path)

(defun run-command-asynchronously (arguments input)
  "Run ARGUMENTS with INPUT on standard input, servicing keystrokes meanwhile."
  (let* ((stem (format nil "~Amaxwell-command-~A"
                       (uiop:temporary-directory) (random-value 8)))
         (input-path (pathname (concatenate 'string stem ".in")))
         (output-path (pathname (concatenate 'string stem ".out")))
         (error-path (pathname (concatenate 'string stem ".err"))))
    (unwind-protect
         (progn
           (write-encoded-file input-path (or input ""))
           (let ((process (sb-ext:run-program
                            (first arguments) (rest arguments)
                            :search t
                            :input input-path
                            :output output-path
                            :error error-path
                            :if-output-exists :supersede
                            :if-error-exists :supersede
                            :wait nil)))
             (unwind-protect
                  (progn
                    (loop while (sb-ext:process-alive-p process)
                          do (funcall *input-poll*)
                             (sleep 0.02))
                    (sb-ext:process-wait process)
                    (values (uiop:read-file-string output-path)
                            (uiop:read-file-string error-path)
                            (or (sb-ext:process-exit-code process) 1)))
               (sb-ext:process-close process))))
      ;; The input line must go away before the caller prints anything else.
      (when *input-hide* (funcall *input-hide*))
      (dolist (path (list input-path output-path error-path))
        (when (probe-file path) (delete-file path))))))

(defun run-command (arguments &key input)
  "Run ARGUMENTS with INPUT on its standard input. Returns the command's
output, its error output, and its exit status."
  (if (null *input-poll*)
      (multiple-value-bind (output error-output status)
          (uiop:run-program arguments
                            :input (make-string-input-stream (or input ""))
                            :output :string :error-output :string
                            :ignore-error-status t)
        (values output error-output status))
      (run-command-asynchronously arguments input)))
