(in-package #:maxwell)

;;; The status bar: one dim row directly above the prompt line, showing which
;;; provider and model is answering and what Maxwell is doing -- idle,
;;; thinking, responding, tooling, summarizing, or saving.
;;;
;;; The prompt draws it, and anything that prints into the transcript takes it
;;; away first, so it never disturbs what is already on the screen. It appears
;;; only on an interactive terminal; /statusbar turns it off and on.

(defvar *status-bar-enabled* t
  "When true, the status bar is drawn above the prompt line.")

(defvar *status-bar-state* :idle
  "What Maxwell is doing: :IDLE, :THINKING, :RESPONDING, :TOOLING,
:SUMMARIZING, or :SAVING.")

(defvar *status-bar-detail* nil
  "Extra text after the state, such as the tool being run.")

(defvar *status-bar-visible* nil
  "True while the status row is on screen directly above the cursor.")

(defvar *status-bar-drawn* ""
  "The text last written to the status row.")

(defun set-activity (state &optional detail)
  "Say what Maxwell is doing, for the status bar. STATE is one of :IDLE,
:THINKING, :RESPONDING, :TOOLING, :SUMMARIZING, or :SAVING; DETAIL, such as the
name of the tool being run, is shown after it."
  (setf *status-bar-state* state
        *status-bar-detail* detail))

(defun status-bar-host-label (url)
  "A short provider name guessed from a base URL: api.openai.com -> openai."
  (let* ((slash (or (search "//" url) -2))
         (rest (subseq url (+ slash 2)))
         (end (or (position #\/ rest) (length rest)))
         (host (subseq rest 0 end))
         (colon (position #\: host))
         (host (if colon (subseq host 0 colon) host))
         (labels (uiop/utility:split-string host :separator '(#\.)))
         (candidate (if (cdr labels) (car (last labels 2)) host)))
    (cond ((every #'digit-char-p candidate) host)
          ((member candidate '("api" "www" "co" "com" "io") :test #'string=)
           (or (car (last labels 3)) candidate))
          (t candidate))))

(defun status-bar-provider (client)
  "A short name for CLIENT's provider."
  (cond ((null client) "-")
        ((client-auth-provider client)
         (string-downcase (symbol-name (client-auth-provider client))))
        (t (status-bar-host-label (client-base-url client)))))

(defun status-bar-model (client)
  "CLIENT's model, or a placeholder while there is none."
  (let ((model (and client (client-model client))))
    (if (and model (plusp (length model))) model "no model")))

(defun status-bar-state-name ()
  (if (and *status-bar-state* (symbolp *status-bar-state*))
      (string-downcase (symbol-name *status-bar-state*))
      "idle"))

(defun status-bar-text (client)
  "The status bar's plain text."
  (if *status-bar-detail*
      (format nil "~A/~A  ~A · ~A" (status-bar-provider client)
              (status-bar-model client) (status-bar-state-name)
              *status-bar-detail*)
      (format nil "~A/~A  ~A" (status-bar-provider client)
              (status-bar-model client) (status-bar-state-name))))

(defun status-bar-terminal-p (stream)
  (and *status-bar-enabled* (ignore-errors (interactive-stream-p stream))))

(defun status-bar-row (client columns)
  "The status bar as one styled row, clipped to COLUMNS."
  (let* ((text (status-bar-text client))
         (limit (max 8 (- (max 10 (or columns 80)) 2))))
    (when (> (length text) limit)
      (setf text (concatenate 'string (subseq text 0 (max 1 (1- limit))) "…")))
    (styled :dim text)))

(defun status-bar-hide (&optional (stream *standard-output*))
  "Clear the status row, leaving the cursor at its start so the next output
starts where the status bar was."
  (when (and *status-bar-visible* (status-bar-terminal-p stream))
    (setf *status-bar-visible* nil)
    (format stream "~C[A~C~C[2K" #\Esc #\Return #\Esc)
    (finish-output stream)))

(defun status-bar-refresh (&optional (stream *standard-output*) columns client)
  "Redraw the status row in place when its text changed. Does nothing while
the row is off screen; the next prompt redraw puts it back."
  (when (and *status-bar-visible* (status-bar-terminal-p stream))
    (let ((row (status-bar-row client columns)))
      (unless (string= row *status-bar-drawn*)
        (setf *status-bar-drawn* row)
        (format stream "~C[A~C~C[2K~A~C[B~C" #\Esc #\Return #\Esc row #\Esc
                #\Return)
        (finish-output stream)))))

(defun status-bar-show (&optional (stream *standard-output*) columns client)
  "Make the status row sit directly above the cursor, drawing it when it is
not there yet. The caller draws its prompt on the row below afterwards."
  (when (status-bar-terminal-p stream)
    (let ((row (status-bar-row client columns)))
      (cond
        (*status-bar-visible*
         (unless (string= row *status-bar-drawn*)
           (setf *status-bar-drawn* row)
           (format stream "~C[A~C~C[2K~A~C[B~C" #\Esc #\Return #\Esc row #\Esc
                   #\Return)))
        (t
         (setf *status-bar-visible* t
               *status-bar-drawn* row)
         ;; CR then ESC[2K, as everywhere else: the terminal is in raw mode, so
         ;; the newline needs its own carriage return.
         (format stream "~C~C[2K~A~C~%" #\Return #\Esc row #\Return)))
      (finish-output stream))))

(defun status-bar-forget ()
  "Forget the status row: the screen was cleared, so nothing of it is left."
  (setf *status-bar-visible* nil
        *status-bar-drawn* ""))

(defun status-bar-on ()
  "Turn the status bar on."
  (setf *status-bar-enabled* t)
  t)

(defun status-bar-off ()
  "Turn the status bar off, clearing the row it occupies."
  (status-bar-hide)
  (setf *status-bar-enabled* nil
        *status-bar-visible* nil
        *status-bar-drawn* "")
  nil)

(defun status-bar-toggle ()
  "Turn the status bar off when it is on, and on when it is off."
  (if *status-bar-enabled* (status-bar-off) (status-bar-on)))

(defun status-bar-command (line client)
  "Run a /statusbar line: on, off, or a toggle."
  (let* ((text (string-trim '(#\Space #\Tab #\Newline) line))
         (argument (string-trim '(#\Space #\Tab)
                               (subseq text (min (length text) 11)))))
    (cond ((string-equal argument "on") (status-bar-on))
          ((string-equal argument "off") (status-bar-off))
          (t (status-bar-toggle)))
    (status-bar-show *standard-output* nil client)))
