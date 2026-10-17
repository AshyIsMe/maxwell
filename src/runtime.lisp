(in-package #:maxwell)

(defparameter *base-system-prompt*
  "You are Maxwell, a small and capable coding agent running inside SBCL.

You have exactly one tool: COMMON_LISP. Use it to inspect, edit, compile, and test the project. Tool calls share one live Lisp process, so definitions and loaded systems persist between calls. Prefer portable Common Lisp; SBCL, ASDF, and UIOP are available when necessary. Work directly toward the user's goal and explain the completed result concisely.

Use ordinary Lisp file and directory functions, ASDF/UIOP, or libraries already present in the image. Do not try to call a shell: Common Lisp is your computational environment. Evaluate small, legible forms and inspect results before making risky changes. You are not sandboxed beyond the operating-system permissions of this process.

To preserve the entire live agent—including conversation history, loaded code, and Lisp state—as a resumable executable SBCL image, evaluate (maxwell:request-image \"path\"). Maxwell writes it only after the current run reaches a safe boundary.")

(defstruct (agent (:constructor %make-agent))
  client
  (history '() :type list)
  (workspace (uiop:getcwd) :type pathname)
  (system-prompt *base-system-prompt* :type string)
  model-function)

(defparameter *pending-image* nil)
(defparameter *agent* nil)

(defun workspace-path (path)
  "Resolve PATH relative to the active agent's workspace."
  (merge-pathnames path
                   (if *agent*
                       (agent-workspace *agent*)
                       *default-pathname-defaults*)))

(defun request-image (path)
  "Request an executable SBCL image at the end of the current agent run."
  (setf *pending-image* (namestring (workspace-path path)))
  (format nil "Maxwell will save its state to ~A after this turn." *pending-image*))

(defun render-values (values)
  (let ((*print-length* 80)
        (*print-level* 12)
        (*print-pretty* t))
    (with-output-to-string (stream)
      (cond
        ((null values) (write-string "; No values" stream))
        ((null (rest values)) (prin1 (first values) stream))
        (t (format stream "~{~S~^~%~}" values))))))

(defun read-forms (code)
  (let ((forms '())
        (position 0)
        (eof (gensym "EOF"))
        (*read-eval* t))
    (loop
      (multiple-value-bind (form end)
          (read-from-string code nil eof :start position)
        (when (eq form eof) (return (nreverse forms)))
        (push form forms)
        (setf position end)))))

(defun eval-lisp (code &key (package (find-package '#:maxwell/workspace))
                             (directory (and *agent* (agent-workspace *agent*))))
  "Evaluate every form in CODE, retaining definitions in PACKAGE.
Returns a printable transcript rather than signaling evaluation errors."
  (let ((output (make-string-output-stream))
        (values '()))
    (handler-case
        (let ((*package* package)
              (*standard-output* output)
              (*error-output* output)
              (*trace-output* output)
              (*default-pathname-defaults* (or directory *default-pathname-defaults*)))
          (dolist (form (read-forms code))
            (setf values (multiple-value-list (eval form))))
          (let ((printed (render-values values))
                (transcript (get-output-stream-string output)))
            (if (zerop (length transcript))
                printed
                (format nil "~A~%=> ~A" transcript printed))))
      (error (condition)
        (let ((transcript (get-output-stream-string output)))
          (format nil "~@[~A~%~]~A: ~A"
                  (and (plusp (length transcript)) transcript)
                  (type-of condition)
                  condition))))))

(defun save-image (path &key (executable t))
  "Save the entire running Maxwell state and terminate this SBCL process."
  #+sbcl
  (progn
    (setf *pending-image* nil)
    (ensure-directories-exist path)
    (format t "~&Saving Maxwell image to ~A~%" path)
    (finish-output)
    (sb-ext:save-lisp-and-die path
                              :executable executable
                              :save-runtime-options t
                              :toplevel #'image-main))
  #-sbcl
  (declare (ignore path executable))
  #-sbcl
  (error "Image saving is currently implemented for SBCL only."))
