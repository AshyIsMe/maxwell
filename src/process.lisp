(in-package #:maxwell)

;;; Provider requests run in a child process, which is polled rather than
;;; waited on, so that output can be watched as it arrives. A child used to be
;;; asynchronous only at the interactive prompt, whose one thread had to keep
;;; servicing keystrokes; now it is always polled, and *INPUT-POLL* merely lets
;;; the UI thread do its work between polls. Requests run on a worker thread,
;;; so blocking there -- and streaming there -- never touches the prompt.
;;;
;;; The same machinery serves the background compactor: START-COMMAND launches
;;; a child without waiting, COMMAND-FINISHED-P polls it, and FINISH-COMMAND or
;;; CANCEL-COMMAND collects it.

(defvar *input-poll* nil
  "When non-NIL, a function called repeatedly while a subprocess runs, so that
the UI thread can keep handling keystrokes and queued work.")

(defvar *input-hide* nil
  "When non-NIL, a function called once when a subprocess finishes, so that the
interactive prompt can erase its input line before other output appears.")

(defun write-encoded-file (path text)
  (with-open-file (stream path :direction :output :if-exists :supersede
                               :if-does-not-exist :create
                               :external-format :utf-8)
    (write-string text stream))
  path)

(defun write-private-file (path text)
  "Create PATH readable only by its owner and write TEXT to it. The file is
restricted before any of TEXT reaches it."
  (with-open-file (stream path :direction :output :if-exists :error
                               :if-does-not-exist :create
                               :external-format :utf-8)
    (protect-file path)
    (write-string text stream))
  path)

(defun private-temporary-path (kind)
  "A fresh, unpredictable temporary pathname for a private file of KIND."
  (pathname (format nil "~Amaxwell-~A-~A" (uiop:temporary-directory) kind
                    (random-value 12))))

(defun write-private-files (files)
  "Write each (PATH . TEXT) in FILES owner-only. If one fails, those already
written are deleted before the error propagates."
  (handler-bind ((error (lambda (condition)
                          (declare (ignore condition))
                          (delete-private-files files))))
    (loop for (path . text) in files do (write-private-file path text))))

(defun delete-private-files (files)
  (loop for (path) in files
        when (probe-file path) do (delete-file path)))

(defstruct (command-job (:constructor %make-command-job))
  "A child process started by START-COMMAND, with the temporary files that
hold its input and output. It lives only in the running process."
  process
  input-path
  output-path
  error-path
  ;; (PATH . TEXT) files, such as a credential header, that exist only while
  ;; the command runs.
  (private-files '()))

(defun start-command (arguments &key input private-files)
  "Start ARGUMENTS with INPUT on standard input and return a COMMAND-JOB at
once, without waiting for the command to finish. PRIVATE-FILES are (PATH . TEXT)
pairs written owner-only before the command starts and deleted with the job."
  (let* ((stem (format nil "~Amaxwell-command-~A"
                       (uiop:temporary-directory) (random-value 8)))
         (job (%make-command-job
                :input-path (pathname (concatenate 'string stem ".in"))
                :output-path (pathname (concatenate 'string stem ".out"))
                :error-path (pathname (concatenate 'string stem ".err"))
                :private-files private-files)))
    (handler-bind ((error (lambda (condition)
                            (declare (ignore condition))
                            (delete-command-files job))))
      (write-private-files private-files)
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
    (when (and path (probe-file path)) (delete-file path)))
  (delete-private-files (command-job-private-files job)))

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

(defun command-output-so-far (job)
  "The text JOB's process has written so far, or NIL when it cannot be read
yet: the file is not there, or a partial character is still arriving."
  (handler-case
      (let ((path (command-job-output-path job)))
        (when (probe-file path) (uiop:read-file-string path)))
    (error () nil)))

(defparameter *command-poll-interval* 0.02
  "Seconds between polls of a child process while its output is watched.")

(defun run-command-watched (arguments input private-files watch)
  "Run ARGUMENTS with INPUT on standard input, calling WATCH with each new
piece of the command's output as it arrives, so a streamed reply is seen while
it is still coming. The UI thread's poll hook, when there is one, runs between
polls."
  (unwind-protect
       (let ((job (start-command arguments :input input
                                           :private-files private-files))
             (finished nil)
             (seen 0))
         (unwind-protect
              (progn
                (loop until (command-finished-p job)
                      do (when watch
                           (let ((text (command-output-so-far job)))
                             (when (and text (> (length text) seen))
                               (funcall watch (subseq text seen))
                               (setf seen (length text)))))
                         (when *input-poll* (funcall *input-poll*))
                         (sleep *command-poll-interval*))
                (multiple-value-prog1 (finish-command job)
                  (setf finished t)))
           (unless finished (cancel-command job))))
    ;; The input line must go away before the caller prints anything else.
    (when *input-hide* (funcall *input-hide*))))

(defun run-command (arguments &key input private-files watch)
  "Run ARGUMENTS with INPUT on its standard input. Returns the command's
output, its error output, and its exit status. PRIVATE-FILES are (PATH . TEXT)
pairs that exist, owner-only, only while the command runs. WATCH, when non-NIL,
is called with each new piece of the output as it arrives, which also makes the
command polled rather than waited on."
  (cond
    (watch (run-command-watched arguments input private-files watch))
    ((null *input-poll*)
     ;; Nothing to service: let UIOP wait for the child itself.
     (unwind-protect
          (progn
            (write-private-files private-files)
            (multiple-value-bind (output error-output status)
                (uiop:run-program arguments
                                :input (make-string-input-stream (or input ""))
                                :output :string :error-output :string
                                :ignore-error-status t)
              (values output error-output status)))
       (delete-private-files private-files)))
    (t (run-command-watched arguments input private-files nil))))
