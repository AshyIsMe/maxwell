(in-package #:maxwell)

(defvar *prompt-history* '()
  "Prompts submitted at the interactive prompt, most recent first.")

(defparameter *prompt-history-limit* 200
  "Maximum number of remembered prompts.")

(defvar *prompt-kill* ""
  "Text removed by the most recent kill command (Ctrl-U, Ctrl-W, Ctrl-K).")

(defun remember-prompt (text)
  "Remember TEXT for Up/Down recall, ignoring blanks and immediate repeats."
  (unless (or (zerop (length (string-trim '(#\Space #\Tab) text)))
              (and *prompt-history* (string= text (first *prompt-history*))))
    (push text *prompt-history*)
    (when (> (length *prompt-history*) *prompt-history-limit*)
      (setf *prompt-history* (subseq *prompt-history* 0 *prompt-history-limit*))))
  text)

(defun prompt-history-lines ()
  "Editing buffer for one prompt: slot 0 is the draft line, slots 1..N are
remembered prompts, most recent first."
  (concatenate 'vector (vector "") *prompt-history*))

(defun delete-range (text start end)
  "TEXT without the characters in [START, END)."
  (concatenate 'string (subseq text 0 start) (subseq text end)))

(defun word-start-before (text cursor)
  "Index where the whitespace-delimited word before CURSOR begins (Ctrl-W)."
  (let ((index cursor)
        (blank '(#\Space #\Tab)))
    (loop while (and (plusp index) (member (char text (1- index)) blank))
          do (decf index))
    (loop while (and (plusp index) (not (member (char text (1- index)) blank)))
          do (decf index))
    index))

(defun terminal-key (stream)
  "Read one key from STREAM: a character, NIL at end of file, or a keyword for
an escape sequence. Ctrl-T is :TOGGLE-TOOLS everywhere."
  (let ((character (read-char stream nil nil)))
    (cond
      ((eql character (code-char 20)) :toggle-tools)
      ((and character (char= character #\Escape))
        (let ((sequence (make-string-output-stream)))
          (loop for index below 12
                do (loop repeat 5 until (listen stream) do (sleep 0.01))
                   (unless (listen stream) (return))
                   (let ((next (read-char stream)))
                     (write-char next sequence)
                     (when (and (plusp index)
                                (or (alpha-char-p next) (char= next #\~)))
                       (return))))
          (let ((code (get-output-stream-string sequence)))
            (cond ((string= code "[5~") :page-up)
                  ((string= code "[6~") :page-down)
                  ((member code '("[A" "OA") :test #'string=) :up)
                  ((member code '("[B" "OB") :test #'string=) :down)
                  ((string= code "[D") :left)
                  ((string= code "[C") :right)
                  ((member code '("[H" "[1~" "OH") :test #'string=) :home)
                  ((member code '("[F" "[4~" "OF") :test #'string=) :end)
                  ((string= code "[3~") :delete)
                  (t :escape)))))
      (t character))))

(defun draw-viewer (viewer rows output)
  (format output "~C[H~C[2J" #\Escape #\Escape)
  (let* ((start (viewer-start viewer))
         (count (viewer-row-count viewer))
         (end (min count (+ start (viewer-page-size viewer)))))
    (loop for k from start below end
          do (write-line (viewer-row viewer k) output))
    (format output "~C[~D;1H~A" #\Escape rows
            (styled :dim
                    (format nil "Transcript ~D-~D/~D~:[~;+~] | PgUp/PgDn scroll; Ctrl-T ~
                                 ~:[expand~;collapse~] tools; Esc returns"
                            (min count (1+ start)) end count
                            (viewer-more-above-p viewer)
                            (eq *tool-display* :full)))))
  (finish-output output))

(defun browse-transcript (input output &optional (entries *session-entries*))
  "Page through ENTRIES on the alternate screen, rendering older entries only
as the user scrolls back to them; return the exit key. Ctrl-T switches the
tool display and keeps the entry at the top of the screen where it is; the
main screen is redrawn in the new mode on the way out."
  (multiple-value-bind (rows columns) (terminal-size)
    (let ((viewer (open-viewer entries (1- columns) (- rows 2)))
          (mode *tool-display*))
      (unwind-protect
           (progn
             (format output "~C[?1049h" #\Escape)
             (loop
               (draw-viewer viewer rows output)
               (let ((key (terminal-key input)))
                 (case key
                   (:page-up (viewer-scroll viewer -1))
                   (:page-down
                    (if (viewer-at-bottom-p viewer)
                        (return :escape)
                        (viewer-scroll viewer 1)))
                   (:toggle-tools
                    (toggle-tool-display)
                    (clear-row-cache)
                    (viewer-rerender viewer))
                   (otherwise (return key))))
               ;; A resized terminal invalidates every rendered row.
               (multiple-value-bind (new-rows new-columns) (terminal-size)
                 (unless (and (= new-rows rows) (= new-columns columns))
                   (setf rows new-rows columns new-columns)
                   (clear-row-cache)
                   (viewer-rerender viewer :width (1- columns) :page-size (- rows 2))))))
        (format output "~C[?1049l" #\Escape)
        (finish-output output)
        (unless (eq mode *tool-display*)
          (redraw-transcript output))))))

(defun terminal-columns ()
  (nth-value 1 (terminal-size)))

(defun interactive-terminal-state ()
  "The controlling terminal's saved settings, or NIL when Maxwell is not on an
interactive terminal."
  (and (interactive-stream-p *standard-input*)
       (interactive-stream-p *standard-output*)
       (not (equal (uiop:getenv "TERM") "dumb"))
       (ignore-errors (string-trim '(#\Newline #\Return)
                                   (terminal-command "-g")))))

(defvar *prompt-draft* ""
  "Text typed while the model was working but not submitted. It is carried into
the next prompt instead of being discarded.")

;;; The prompt is a small state machine so that the same editing keys work both
;;; at the prompt and while the model is working.

(defstruct (prompt-editor (:constructor %make-prompt-editor))
  agent input output
  (prompt "maxwell> ")
  (text "") (cursor 0)
  (lines (vector "")) (index 0)
  (columns 80))

(defun make-prompt-editor (agent input output &key (prompt "maxwell> ") (text ""))
  (%make-prompt-editor :agent agent :input input :output output :prompt prompt
                       :text text :cursor (length text)
                       :lines (prompt-history-lines)
                       :columns (terminal-columns)))

(defun editor-visible-width (editor)
  (max 1 (- (prompt-editor-columns editor)
            (visible-length (prompt-editor-prompt editor)) 2)))

(defun session-client ()
  "The client of the session's agent, for the status bar, or NIL."
  (and *agent* (agent-client *agent*)))

(defun editor-redraw (editor)
  "Draw the status bar and then the prompt line under it."
  (let* ((output (prompt-editor-output editor))
         (text (prompt-editor-text editor))
         (cursor (prompt-editor-cursor editor))
         (width (editor-visible-width editor))
         (start (max 0 (- cursor width)))
         (end (min (length text) (+ start width))))
    (status-bar-show output (prompt-editor-columns editor) (session-client))
    (format output "~C~C[2K~A~A" #\Return #\Escape (prompt-editor-prompt editor)
            (subseq text start end))
    (when (< cursor end)
      (format output "~C[~DD" #\Escape (- end cursor)))
    (finish-output output)))

(defun editor-erase (editor)
  (let ((output (prompt-editor-output editor)))
    (format output "~C~C[2K" #\Return #\Escape)
    (finish-output output)))

(defun editor-recall (editor position)
  (setf (prompt-editor-index editor) position
        (prompt-editor-text editor) (aref (prompt-editor-lines editor) position)
        (prompt-editor-cursor editor) (length (prompt-editor-text editor))))

(defun editor-remove-range (editor start end)
  (setf (prompt-editor-text editor) (delete-range (prompt-editor-text editor) start end)
        (prompt-editor-cursor editor) start))

(defun editor-kill (editor start end)
  (let ((killed (subseq (prompt-editor-text editor) start end)))
    (when (plusp (length killed)) (setf *prompt-kill* killed))
    (editor-remove-range editor start end)))

(defun editor-insert (editor string)
  (let ((text (prompt-editor-text editor))
        (cursor (prompt-editor-cursor editor)))
    (setf (prompt-editor-text editor)
          (concatenate 'string (subseq text 0 cursor) string (subseq text cursor))
          (prompt-editor-cursor editor) (+ cursor (length string)))))

(defun editor-key (editor key)
  "Handle one KEY in EDITOR. Returns :SUBMIT, :CANCEL, :BROWSE, :TOGGLE, or
:CONTINUE."
  (let ((text (prompt-editor-text editor))
        (cursor (prompt-editor-cursor editor)))
    (cond
      ((null key) :cancel)
      ((eq key :page-up) :browse)
      ((eq key :toggle-tools) :toggle)
      ((and (characterp key) (find key '(#\Newline #\Return))) :submit)
      ;; Ctrl-D deletes forward, or ends the session on an empty line.
      ((and (characterp key) (= (char-code key) 4))
       (if (plusp (length text))
           (progn (when (< cursor (length text))
                    (editor-remove-range editor cursor (1+ cursor)))
                  :continue)
           :cancel))
      ;; History: Up/Ctrl-P recall older prompts, Down/Ctrl-N return.
      ((or (eq key :up) (eql key (code-char 16)))
       (let ((index (prompt-editor-index editor)))
         (when (zerop index) (setf (aref (prompt-editor-lines editor) 0) text))
         (when (< index (1- (length (prompt-editor-lines editor))))
           (editor-recall editor (1+ index))))
       :continue)
      ((or (eq key :down) (eql key (code-char 14)))
       (let ((index (prompt-editor-index editor)))
         (when (plusp index) (editor-recall editor (1- index))))
       :continue)
      ;; Movement.
      ((or (eq key :left) (eql key (code-char 2)))
       (setf (prompt-editor-cursor editor) (max 0 (1- cursor))) :continue)
      ((or (eq key :right) (eql key (code-char 6)))
       (setf (prompt-editor-cursor editor) (min (length text) (1+ cursor))) :continue)
      ((or (eq key :home) (eql key (code-char 1)))
       (setf (prompt-editor-cursor editor) 0) :continue)
      ((or (eq key :end) (eql key (code-char 5)))
       (setf (prompt-editor-cursor editor) (length text)) :continue)
      ;; Deletion.
      ((and (characterp key) (member (char-code key) '(8 127)))
       (when (plusp cursor) (editor-remove-range editor (1- cursor) cursor)) :continue)
      ((eq key :delete)
       (when (< cursor (length text)) (editor-remove-range editor cursor (1+ cursor)))
       :continue)
      ((eql key (code-char 11)) (editor-kill editor cursor (length text)) :continue)
      ((eql key (code-char 21)) (editor-kill editor 0 cursor) :continue)
      ((eql key (code-char 23))
       (editor-kill editor (word-start-before text cursor) cursor) :continue)
      ;; Editing.
      ((eql key (code-char 25)) (editor-insert editor *prompt-kill*) :continue)
      ((eql key (code-char 12))
       (format (prompt-editor-output editor) "~C[H~C[2J" #\Escape #\Escape)
       :continue)
      ((and (characterp key) (>= (char-code key) 32))
       (editor-insert editor (string key)) :continue)
      (t :continue))))

(defun echo-submitted-prompt (editor)
  "Replace the prompt line with the submitted text, drawn as a user entry."
  (let ((output (prompt-editor-output editor))
        (text (prompt-editor-text editor)))
    (if (zerop (length (string-trim '(#\Space #\Tab) text)))
        (progn (terpri output) (finish-output output))
        (progn (editor-erase editor)
               (print-entry (make-entry :user text) output)))))

(defun stream-fd (stream)
  "The file descriptor under STREAM, following synonym streams, or NIL."
  (loop while (typep stream 'synonym-stream)
        do (setf stream (symbol-value (synonym-stream-symbol stream))))
  (and (typep stream 'sb-sys:fd-stream) (sb-sys:fd-stream-fd stream)))

(defun await-key (input idle)
  "Return once a key, or end of file, is waiting on INPUT, calling IDLE about
every 50 ms meanwhile. Without IDLE, or on a stream that is not a file, return
at once and let the read block."
  (let ((fd (and idle (stream-fd input))))
    (when fd
      ;; LISTEN sees buffered keys; the descriptor also reports end of file,
      ;; which LISTEN does not.
      (loop until (or (listen input) (sb-sys:wait-until-fd-usable fd :input 0.05))
            do (funcall idle)))))

(defun read-terminal-prompt (agent input output &key idle)
  "Read one prompt line with readline-style editing: Up/Down recall previous
prompts, PgUp opens the transcript, Ctrl-T expands or collapses tool calls,
and Ctrl-A/E/B/F/U/W/K/Y/L edit the line. IDLE, when given, is called while
no key arrives, so background work such as compaction continues."
  (declare (ignorable agent))
  (set-activity :idle)
  (let ((editor (make-prompt-editor agent input output :text *prompt-draft*)))
    (setf *prompt-draft* "")
    (editor-redraw editor)
    (loop for key = (progn (await-key input idle) (terminal-key input))
          for action = (editor-key editor key)
          do (case action
               (:browse (browse-transcript input output))
               (:toggle (toggle-tool-display) (redraw-transcript output))
               (:submit
                (echo-submitted-prompt editor)
                (remember-prompt (prompt-editor-text editor))
                (return (prompt-editor-text editor)))
               (:cancel (return nil)))
             (editor-redraw editor))))

;;; Steering input: while the model works the prompt stays live, so the user can
;;; browse the transcript with PgUp/PgDn and type prompts that reach the model
;;; before its next request.

(defstruct (steering-session (:constructor %make-steering-session))
  editor
  (shown nil))

(defun make-steering-session (agent input output)
  (%make-steering-session
    :editor (make-prompt-editor agent input output :prompt "steer> ")))

(defun steering-draft (session)
  (prompt-editor-text (steering-session-editor session)))

(defun steering-show (session)
  "Draw the steering prompt, and with it the region above: the status row and,
while the model thinks, the live reasoning trace."
  (let ((editor (steering-session-editor session)))
    (editor-redraw editor)
    (setf (steering-session-shown session) t)))

(defun steering-hide (session)
  "Erase the steering prompt so other output starts on a clean line."
  (when (steering-session-shown session)
    (editor-erase (steering-session-editor session))
    (setf (steering-session-shown session) nil)))

(defun steering-submit (session)
  "Send the steering prompt to the model and echo it."
  (let* ((editor (steering-session-editor session))
         (output (prompt-editor-output editor))
         (text (prompt-editor-text editor)))
    (setf (prompt-editor-text editor) "" (prompt-editor-cursor editor) 0)
    (editor-erase editor)
    (setf (steering-session-shown session) nil)
    (when (plusp (length (string-trim '(#\Space #\Tab) text)))
      (remember-prompt text)
      (queue-steering text)
      (show-entry *session-entries* (1- (length *session-entries*)) output))))

(defun service-steering-input (session)
  "Handle keystrokes waiting while the model works. Returns T when any arrived.
PgUp/PgDn browse the transcript and Ctrl-T redraws it with tool calls expanded
or collapsed; the steering line is drawn again afterwards."
  (let* ((editor (steering-session-editor session))
         (input (prompt-editor-input editor))
         (handled nil)
         (prompt (if *busy-text*
                     (format nil "~A steer> " (styled :dim *busy-text*))
                     "steer> ")))
    ;; A turn waiting for summaries shows a spinner before the prompt.
    (unless (string= prompt (prompt-editor-prompt editor))
      (setf (prompt-editor-prompt editor) prompt
            (steering-session-shown session) nil))
    (loop while (listen input)
          do (setf handled t)
             (let ((key (terminal-key input)))
               (if (member key '(:page-up :page-down))
                   (browse-transcript input (prompt-editor-output editor))
                   (case (editor-key editor key)
                     (:submit (steering-submit session))
                     (:browse (browse-transcript input (prompt-editor-output editor)))
                     (:toggle (toggle-tool-display)
                      (redraw-transcript (prompt-editor-output editor)))
                     (otherwise nil)))))
    (if handled
        (progn (editor-redraw editor)
               (setf (steering-session-shown session) t))
        (steering-show session))
    handled))

(defun read-interactive-prompt (agent)
  "Read the next prompt line, or NIL at end of input. Ctrl-C discards the line
being typed and returns an empty one."
  (handler-case (read-interactive-line agent)
    (sb-sys:interactive-interrupt ()
      (fresh-line)
      "")))

(defun read-interactive-line (agent)
  (let ((state (interactive-terminal-state)))
    (if state
        (unwind-protect
             (progn
               (terminal-command "-icanon" "-echo" "min" "1" "time" "0")
               (read-terminal-prompt agent *standard-input* *standard-output*
                                     :idle (lambda () (tick-compactor agent))))
          (terminal-command state))
        (progn
          (format t "~&maxwell> ")
          (finish-output)
          (let ((line (read-line *standard-input* nil nil)))
            (when line (remember-prompt line))
            line)))))
