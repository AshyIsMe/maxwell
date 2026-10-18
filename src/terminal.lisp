(in-package #:maxwell)

(defparameter *prompt-history* '()
  "Prompts submitted at the interactive prompt, most recent first.")

(defparameter *prompt-history-limit* 200
  "Maximum number of remembered prompts.")

(defparameter *prompt-kill* ""
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

(defun transpose-characters (text cursor)
  "Swap the characters around CURSOR (Ctrl-T). Returns (VALUES TEXT CURSOR)."
  (let* ((at-end (>= cursor (length text)))
         (left (if at-end (- cursor 2) (1- cursor)))
         (right (if at-end (1- cursor) cursor)))
    (if (or (minusp left) (>= right (length text)) (>= left right))
        (values text cursor)
        (values (concatenate 'string
                             (subseq text 0 left)
                             (string (char text right))
                             (subseq text (1+ left) right)
                             (string (char text left))
                             (subseq text (1+ right)))
                (1+ right)))))

(defun terminal-command (&rest arguments)
  ;; Use the controlling terminal, not redirected standard input.
  ;; UIOP starts the child in a separate process group. A background stty
  ;; receives SIGTTOU when changing terminal settings, even with echo off.
  ;; Ignore that signal in the child so setup and restoration can complete.
  (uiop:run-program (append '("sh" "-c"
                             "trap '' TTOU; exec stty -F /dev/tty \"$@\""
                             "maxwell-stty") arguments)
                    :output :string :error-output :string))

(defun transcript-text (agent)
  (with-output-to-string (out)
    (dolist (message (agent-history agent))
      (format out "~A:~%~A~%" (get-key message "role" "message")
              (message-content message))
      (let ((calls (get-key message "tool_calls")))
        (when (vectorp calls)
          (loop for call across calls
                for function = (get-key call "function")
                do (format out "[~A] ~A~%" (get-key function "name")
                           (get-key function "arguments")))))
      (terpri out))))

(defun transcript-lines (text width)
  "Wrap text for display, stripping terminal control characters."
  (let ((lines '()) (line (make-string-output-stream)) (column 0))
    (labels ((flush-line ()
               (push (get-output-stream-string line) lines)
               (setf column 0))
             (emit (character)
               (when (= column width) (flush-line))
               (write-char character line)
               (incf column)))
      (loop for character across text
            do (cond ((char= character #\Newline) (flush-line))
                     ((char= character #\Tab)
                      (dotimes (i (- 8 (mod column 8))) (emit #\Space)))
                     ((and (>= (char-code character) 32)
                           (/= (char-code character) 127)
                           (not (<= 128 (char-code character) 159)))
                      (emit character))))
      (when (or (plusp column) (null lines)) (flush-line)))
    (coerce (nreverse lines) 'vector)))

(defun transcript-page-start (start direction count page-size)
  (max 0 (min (max 0 (- count page-size))
              (+ start (* direction page-size)))))

(defun terminal-key (stream)
  (let ((character (read-char stream nil nil)))
    (if (and character (char= character #\Escape))
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
                  (t :escape))))
        character)))

(defun browse-transcript (agent input output)
  "Display a paged snapshot on the alternate screen; return the exit key."
  (let* ((size (ignore-errors
                 (mapcar #'parse-integer
                         (remove "" (uiop:split-string (terminal-command "size"))
                                 :test #'string=))))
         (rows (max 3 (or (first size) 24)))
         (width (max 1 (1- (or (second size) 80))))
         (page-size (- rows 2))
         (lines (transcript-lines (transcript-text agent) width))
         (start (max 0 (- (length lines) (* 2 page-size)))))
    (unwind-protect
         (progn
           (format output "~C[?1049h" #\Escape)
           (loop
             (format output "~C[H~C[2J" #\Escape #\Escape)
             (loop for i from start below (min (length lines) (+ start page-size))
                   do (format output "~A~%" (aref lines i)))
             (format output "~C[~D;1HTranscript ~D-~D/~D | PgUp/PgDn scroll; Esc returns"
                     #\Escape rows (1+ start)
                     (min (length lines) (+ start page-size)) (length lines))
             (finish-output output)
             (let ((key (terminal-key input)))
               (case key
                 (:page-up (setf start (transcript-page-start start -1 (length lines) page-size)))
                 (:page-down
                  (if (= start (max 0 (- (length lines) page-size)))
                      (return :escape)
                      (setf start (transcript-page-start start 1 (length lines) page-size))))
                 (otherwise (return key))))))
      (format output "~C[?1049l" #\Escape)
      (finish-output output))))

(defun terminal-columns ()
  (or (ignore-errors
        (second (mapcar #'parse-integer
                        (remove "" (uiop:split-string (terminal-command "size")
                                                      :test #'string=)))))
      80))

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
            (length (prompt-editor-prompt editor)) 2)))

(defun editor-redraw (editor)
  (let* ((output (prompt-editor-output editor))
         (text (prompt-editor-text editor))
         (cursor (prompt-editor-cursor editor))
         (width (editor-visible-width editor))
         (start (max 0 (- cursor width)))
         (end (min (length text) (+ start width))))
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
  "Handle one KEY in EDITOR. Returns :SUBMIT, :CANCEL, :BROWSE, or :CONTINUE."
  (let ((text (prompt-editor-text editor))
        (cursor (prompt-editor-cursor editor)))
    (cond
      ((null key) :cancel)
      ((eq key :page-up) :browse)
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
      ((eql key (code-char 20))
       (multiple-value-bind (new-text new-cursor) (transpose-characters text cursor)
         (setf (prompt-editor-text editor) new-text
               (prompt-editor-cursor editor) new-cursor))
       :continue)
      ((eql key (code-char 12))
       (format (prompt-editor-output editor) "~C[H~C[2J" #\Escape #\Escape)
       :continue)
      ((and (characterp key) (>= (char-code key) 32))
       (editor-insert editor (string key)) :continue)
      (t :continue))))

(defun read-terminal-prompt (agent input output)
  "Read one prompt line with readline-style editing: Up/Down recall previous
prompts, PgUp opens the transcript, and Ctrl-A/E/B/F/U/W/K/Y/T/L edit the line."
  (let ((editor (make-prompt-editor agent input output :text *prompt-draft*)))
    (setf *prompt-draft* "")
    (editor-redraw editor)
    (loop for key = (terminal-key input)
          for action = (editor-key editor key)
          do (case action
               (:browse (browse-transcript agent input output))
               (:submit
                (terpri output)
                (finish-output output)
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
  "Draw the steering prompt if it is not already on screen."
  (unless (steering-session-shown session)
    (editor-redraw (steering-session-editor session))
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
      (format output "~&steer> ~A~%" text)
      (finish-output output)
      (remember-prompt text)
      (queue-steering text))))

(defun service-steering-input (session)
  "Handle keystrokes waiting while the model works. Returns T when any arrived."
  (let* ((editor (steering-session-editor session))
         (input (prompt-editor-input editor))
         (handled nil))
    (loop while (listen input)
          do (setf handled t)
             (let ((key (terminal-key input)))
               (if (member key '(:page-up :page-down))
                   (browse-transcript (prompt-editor-agent editor) input
                                       (prompt-editor-output editor))
                   (case (editor-key editor key)
                     (:submit (steering-submit session))
                     (:browse (browse-transcript (prompt-editor-agent editor) input
                                                 (prompt-editor-output editor)))
                     (otherwise nil)))))
    (if handled
        (progn (editor-redraw editor)
               (setf (steering-session-shown session) t))
        (steering-show session))
    handled))

(defun read-interactive-prompt (agent)
  (let ((state (interactive-terminal-state)))
    (if state
        (unwind-protect
             (progn
               (terminal-command "-icanon" "-echo" "min" "1" "time" "0")
               (read-terminal-prompt agent *standard-input* *standard-output*))
          (terminal-command state))
        (progn
          (format t "~&maxwell> ")
          (finish-output)
          (let ((line (read-line *standard-input* nil nil)))
            (when line (remember-prompt line))
            line)))))
