(in-package #:maxwell)

;;; The region above the prompt: one dim row showing which provider and model
;;; is answering and what Maxwell is doing -- idle, thinking, responding,
;;; tooling, summarizing, or saving -- and, while the model thinks, the live
;;; reasoning trace it is streaming, drawn above that row.
;;;
;;; The prompt draws the whole region, and anything that prints into the
;;; transcript takes it away first, so it never disturbs what is already on
;;; the screen. It appears only on an interactive terminal; /statusbar turns
;;; it off and on, and with it the live trace.

(defvar *status-bar-enabled* t
  "When true, the region above the prompt is drawn.")

(defvar *status-bar-state* :idle
  "What Maxwell is doing: :IDLE, :THINKING, :RESPONDING, :TOOLING,
:SUMMARIZING, or :SAVING.")

(defvar *status-bar-detail* nil
  "Extra text after the state, such as the tool being run.")

(defvar *status-bar-visible* nil
  "True while the status region is on screen directly above the cursor.")

(defvar *status-bar-drawn* ""
  "The text last written to the status row.")

(defvar *status-bar-drawn-rows* '()
  "Every row last written to the region, oldest first.")

(defvar *status-bar-extra-rows* 0
  "How many live rows are drawn above the status row; 0 when the status row
stands alone.")

(defvar *live-reasoning* nil
  "The reasoning text the model is streaming right now, shown dim above the
status row while it thinks, or NIL when no trace is on screen.")

(defparameter *live-reasoning-rows* 12
  "The most rows of a live reasoning trace shown above the status row.")

(defun set-activity (state &optional detail)
  "Say what Maxwell is doing, for the status bar. STATE is one of :IDLE,
:THINKING, :RESPONDING, :TOOLING, :SUMMARIZING, or :SAVING; DETAIL, such as the
name of the tool being run, is shown after it."
  (setf *status-bar-state* state
        *status-bar-detail* detail))

;;; The live reasoning trace. Streamed pieces arrive through
;;; NOTE-LIVE-REASONING while the model works; the prompt redraws the rows, and
;;; HIDE-REASONING-TRACE takes them away when the model's reply arrives.

(defun %note-live-reasoning (text)
  "Add TEXT to the reasoning trace shown above the prompt while the model
thinks. Returns the trace so far."
  (when (and (stringp text) (plusp (length text)))
    (setf *live-reasoning* (concatenate 'string (or *live-reasoning* "") text)))
  *live-reasoning*)

(defun note-live-reasoning (text)
  "Add streamed reasoning TEXT to the live trace. Pieces arrive on the thread
that runs the request; the trace is drawn by the UI thread, so a worker's piece
is handed over to it."
  (if (ui-thread-p)
      (when (conversation-visible-p) (%note-live-reasoning text))
      (progn (ui-post (lambda () (when (conversation-visible-p) (%note-live-reasoning text))))
             *live-reasoning*)))

(defun hide-reasoning-trace ()
  "Forget the live reasoning trace and take its rows off the screen, so the
model's reply starts on a clean line."
  (setf *live-reasoning* nil)
  (status-bar-hide))

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

(defun status-bar-limit (columns)
  "The most characters a region row may hold at COLUMNS."
  (max 8 (- (max 10 (or columns 80)) 2)))

(defun status-bar-rate ()
  "The output tokens per second of the last finished request, as a short
suffix for the status row, or NIL while none has been measured."
  (let* ((stat (latest-request-stat))
         (rate (and stat (request-stat-tokens-per-second stat))))
    (when (and rate (plusp rate))
      (format nil "~,1F tok/s" rate))))

(defun status-bar-row (client columns)
  "The status bar as one styled row, clipped to COLUMNS. The rate of the last
request, such as 106.5 tok/s, is pinned to the right edge."
  (let* ((text (status-bar-text client))
         (limit (status-bar-limit columns))
         (rate (status-bar-rate))
         ;; One space at least between the text and the rate.
         (room (and rate (- limit (length text) (length rate)))))
    (when rate
      (if (and room (>= room 2))
          (setf text (concatenate 'string text
                                  (make-string room :initial-element #\Space)
                                  rate))
          ;; No room to pin it; drop the rate rather than cut it in half.
          (setf rate nil)))
    (when (> (length text) limit)
      (setf text (concatenate 'string (subseq text 0 (max 1 (1- limit))) "…")))
    (styled :dim text)))

(defun live-reasoning-rows (columns)
  "The live reasoning rows, oldest first, clipped to COLUMNS: the last
*LIVE-REASONING-ROWS* lines of what the model has thought so far."
  (when *live-reasoning*
    (let* ((limit (status-bar-limit columns))
           (lines (text-lines *live-reasoning*))
           (shown (last lines *live-reasoning-rows*)))
      (mapcar (lambda (line) (styled :thinking (clip-line line limit))) shown))))

(defun status-bar-rows (columns client)
  "Every row of the region above the prompt: the live reasoning rows, then the
status row, which stays next to the prompt."
  (append (live-reasoning-rows columns)
          (list (status-bar-row client columns))))

(defun status-bar-region-height ()
  "The rows the region occupies directly above the prompt line."
  (+ 1 *status-bar-extra-rows*))

(defun draw-status-region (stream rows)
  "Draw ROWS as the region directly above the cursor, which is the line the
prompt will be written on; the cursor ends there again, column 0. ROWS is
never empty: its last row is the status row."
  (when *status-bar-visible*
    ;; Back up over the old region before repainting it, so a trace that grew
    ;; or shrank leaves nothing behind.
    (format stream "~C[~DA" #\Esc (status-bar-region-height))
    (format stream "~C~C[J" #\Return #\Esc))
  (dolist (row rows)
    (format stream "~C~C[2K~A~C~C" #\Return #\Esc row #\Return #\Newline))
  (setf *status-bar-extra-rows* (max 0 (1- (length rows)))
        *status-bar-drawn* (car (last rows))
        *status-bar-drawn-rows* rows
        *status-bar-visible* t))

(defun status-bar-hide (&optional (stream *standard-output*))
  "Clear the region, leaving the cursor where the transcript continues so the
next output starts there."
  (when (and *status-bar-visible* (status-bar-terminal-p stream))
    (format stream "~C[~DA" #\Esc (status-bar-region-height))
    (format stream "~C~C[J" #\Return #\Esc)
    (setf *status-bar-visible* nil
          *status-bar-extra-rows* 0
          *status-bar-drawn* ""
          *status-bar-drawn-rows* '())
    (finish-output stream)))

(defun status-bar-show (&optional (stream *standard-output*) columns client)
  "Make the region sit directly above the cursor, drawing it when it is not
there yet or when its rows changed. The caller draws its prompt on the line
below afterwards."
  (when (status-bar-terminal-p stream)
    (let ((rows (status-bar-rows columns client)))
      (unless (and *status-bar-visible* (equal rows *status-bar-drawn-rows*))
        (draw-status-region stream rows)
        (finish-output stream)))))

(defun status-bar-forget ()
  "Forget the status region: the screen was cleared, so nothing of it is left."
  (setf *status-bar-visible* nil
        *status-bar-extra-rows* 0
        *status-bar-drawn* ""
        *status-bar-drawn-rows* '()))

(defun status-bar-on ()
  "Turn the status bar on."
  (setf *status-bar-enabled* t)
  t)

(defun status-bar-off ()
  "Turn the status bar off, clearing the region it occupies."
  (status-bar-hide)
  (setf *status-bar-enabled* nil
        *status-bar-visible* nil
        *status-bar-extra-rows* 0
        *status-bar-drawn* ""
        *status-bar-drawn-rows* '())
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
