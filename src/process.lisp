(in-package #:maxwell)

;;; Provider requests run in a child process. At the interactive prompt that
;;; child would otherwise block Maxwell's only thread, so it is started
;;; asynchronously and the prompt keeps servicing keystrokes while it runs.
;;; Both hooks are NIL everywhere else, which leaves one-shot, scripted, and
;;; test runs on the plain blocking path.
;;;
;;; The same non-blocking machinery serves the background compactor: START-
;;; COMMAND launches a child without waiting, COMMAND-FINISHED-P polls it, and
;;; FINISH-COMMAND or CANCEL-COMMAND collects it. Nothing here needs a thread.

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

(defstruct (command-job (:constructor %make-command-job))
  "A child process started by START-COMMAND, with the temporary files that
hold its input and output. It lives only in the running process."
  process
  input-path
  output-path
  error-path)

(defun start-command (arguments &key input)
  "Start ARGUMENTS with INPUT on standard input and return a COMMAND-JOB at
once, without waiting for the command to finish."
  (let* ((stem (format nil "~Amaxwell-command-~A"
                       (uiop:temporary-directory) (random-value 8)))
         (job (%make-command-job
                :input-path (pathname (concatenate 'string stem ".in"))
                :output-path (pathname (concatenate 'string stem ".out"))
                :error-path (pathname (concatenate 'string stem ".err")))))
    (handler-bind ((error (lambda (condition)
                            (declare (ignore condition))
                            (delete-command-files job))))
      (write-encoded-file (command-job-input-path job) (or input ""))
      (setf (command-job-process job)
            (sb-ext:run-program (first arguments) (rest arguments)
                                :search t
                                :input (command-job-input-path job)
                                :output (command-job-output-path job)
                                :error (command-job-error-path job)
                                :if-output-exists :supersede
                                :if-error-exists :supersede
                                :wait nil)))
    job))

(defun command-finished-p (job)
  "True once JOB's process has exited."
  (not (sb-ext:process-alive-p (command-job-process job))))

(defun delete-command-files (job)
  (dolist (path (list (command-job-input-path job)
                      (command-job-output-path job)
                      (command-job-error-path job)))
    (when (and path (probe-file path)) (delete-file path))))

(defun finish-command (job)
  "Wait for JOB to exit, delete its files, and return its output, its error
output, and its exit status."
  (let ((process (command-job-process job)))
    (unwind-protect
         (progn
           (sb-ext:process-wait process)
           (values (uiop:read-file-string (command-job-output-path job))
                   (uiop:read-file-string (command-job-error-path job))
                   (or (sb-ext:process-exit-code process) 1)))
      (sb-ext:process-close process)
      (delete-command-files job))))

(defun cancel-command (job)
  "Stop JOB's process if it still runs and delete its files."
  (let ((process (command-job-process job)))
    (when process
      (ignore-errors
        (when (sb-ext:process-alive-p process)
          (sb-ext:process-kill process 15)
          (sb-ext:process-wait process)))
      (ignore-errors (sb-ext:process-close process))))
  (delete-command-files job)
  nil)

(defun run-command-asynchronously (arguments input)
  "Run ARGUMENTS with INPUT on standard input, servicing keystrokes meanwhile."
  (unwind-protect
       (let ((job (start-command arguments :input input))
             (finished nil))
         (unwind-protect
              (progn
                (loop until (command-finished-p job)
                      do (funcall *input-poll*)
                         (sleep 0.02))
                (multiple-value-prog1 (finish-command job)
                  (setf finished t)))
           (unless finished (cancel-command job))))
    ;; The input line must go away before the caller prints anything else.
    (when *input-hide* (funcall *input-hide*))))

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
