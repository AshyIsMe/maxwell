(in-package #:maxwell)

;;; The transcript and its renderer.
;;;
;;; The agent loop *records* transcript entries; only the renderer *prints*
;;; them, live as they arrive, again when Ctrl-T redraws the screen, and lazily
;;; in the PgUp viewer. Rendering turns one entry into terminal rows, cached
;;; per entry for each tool display mode and width. Entries hold plain text:
;;; styling happens here, so it never reaches the model or the log.

(defstruct (transcript-entry
            (:conc-name entry-)
            (:constructor make-entry (kind text &key (ok-p t) id
                                                     (time (get-universal-time)))))
  "One item of the visible transcript. KIND is :user, :assistant, :tool (code
the model ran), :echo (its result), :steer, or :status. OK-P is false for an
error result or an error status. ID is the message's id in the OptChat log,
when it has been logged."
  kind text (ok-p t) id time)

(defun make-entry-vector ()
  (make-array 16 :adjustable t :fill-pointer 0))

(defvar *session-entries* (make-entry-vector)
  "This session's transcript entries, oldest first. Not meaningful when saved:
RESET-TRANSCRIPT empties it at startup and when an image resumes.")

(defvar *tool-display* :summary
  "How tool calls are drawn: :summary (one header line and the last result
lines) or :full (all code and the whole result). Ctrl-T switches it.")

(defvar *show-entries* nil
  "True while recorded entries are printed as they arrive. RUN binds it from
its :PRINT-RESPONSE argument.")

(defvar *older-entries-function* nil
  "When non-NIL, a function of (OLDEST COUNT) that returns up to COUNT entries
older than the entry OLDEST (NIL: older than everything), oldest first. The PgUp
viewer and the redraw call it once *SESSION-ENTRIES* is exhausted, so that
scrolling can continue back through a longer log. NIL means there is nothing
older than the session.")

(defparameter *older-batch-size* 64
  "How many older entries the viewer asks *OLDER-ENTRIES-FUNCTION* for at once.")

(defvar *row-cache* (make-hash-table :test 'eq :weakness :key)
  "Entry -> alist of ((mode width colour) . rows).")

(defvar *row-cache-width* nil
  "The width the row cache was last used with; a new width clears it.")

(defvar *entry-render-count* 0
  "Entries rendered without the cache, for tests of lazy rendering.")

(defun clear-row-cache ()
  (clrhash *row-cache*))

(defun reset-transcript ()
  "Start an empty transcript, as at startup or when an image resumes."
  (setf *session-entries* (make-entry-vector)
        *row-cache-width* nil)
  (clear-row-cache))

(defun toggle-tool-display ()
  "Switch every tool call between summary and full display."
  (setf *tool-display* (if (eq *tool-display* :full) :summary :full)))

;;; Terminal geometry. The controlling terminal is used, not redirected
;;; standard input.

(defun terminal-command (&rest arguments)
  ;; UIOP starts the child in a separate process group. A background stty
  ;; receives SIGTTOU when changing terminal settings, even with echo off.
  ;; Ignore that signal in the child so setup and restoration can complete.
  (uiop:run-program (append '("sh" "-c"
                             "trap '' TTOU; exec stty -F /dev/tty \"$@\""
                             "maxwell-stty") arguments)
                    :output :string :error-output :string))

(defun terminal-size ()
  "The terminal's rows and columns, or 24 and 80 when they are unknown."
  (let ((size (ignore-errors
                (mapcar #'parse-integer
                        (remove "" (uiop:split-string (terminal-command "size"))
                                :test #'string=)))))
    (values (max 3 (or (first size) 24))
            (max 2 (or (second size) 80)))))

(defun live-width (stream)
  "The width to render rows at on STREAM: one less than the terminal's columns,
so a full row never wraps, or NIL (no wrapping) when STREAM is not a terminal."
  (and (interactive-stream-p stream)
       (1- (nth-value 1 (terminal-size)))))

;;; Rendering one entry into rows.

(defun text-lines (text)
  "TEXT's lines, without trailing blank lines."
  (let ((lines (reverse (uiop:split-string text :separator '(#\Newline)))))
    (loop while (and lines (zerop (length (string-trim '(#\Space #\Tab #\Return)
                                                       (first lines)))))
          do (pop lines))
    (nreverse lines)))

(defun clip-line (text limit)
  "TEXT on one line of at most LIMIT characters: control characters become
spaces, and a longer line is cut and ends with an ellipsis."
  (let ((line (map 'string (lambda (character)
                             (if (printable-char-p character) character #\Space))
                   text)))
    (if (> (length line) limit)
        (concatenate 'string (subseq line 0 (max 0 (1- limit))) "…")
        line)))

(defun style-lines (face text)
  "TEXT with each line styled separately, so every line stands on its own."
  (format nil "~{~A~^~%~}"
          (mapcar (lambda (line) (styled face line))
                  (uiop:split-string text :separator '(#\Newline)))))

(defun indent-rows (text width first rest)
  "TEXT wrapped to WIDTH columns, as a list of rows: FIRST prefixes the first
row and REST every later one. With WIDTH NIL, lines are not wrapped."
  (let ((lines (if width
                   (ansi-wrap text (max 1 (- width (visible-length first))))
                   (uiop:split-string text :separator '(#\Newline)))))
    (loop for line across (coerce lines 'vector)
          for prefix = first then rest
          collect (concatenate 'string prefix line))))

(defun lisp-form-end (code start)
  "The index just past the first Lisp form at or after START in CODE, skipping
blanks and comments, and the index where that form begins. It never fails: an
unterminated form ends at the end of CODE."
  (let ((length (length code))
        (i start))
    (labels ((peek (&optional (offset 0))
               (let ((j (+ i offset))) (and (< j length) (char code j))))
             (skip-string ()
               (incf i)
               (loop while (and (< i length) (char/= (char code i) #\"))
                     do (incf i (if (char= (char code i) #\\) 2 1)))
               (setf i (min length (1+ i))))
             (skip-pipe ()
               (incf i)
               (loop while (and (< i length) (char/= (char code i) #\|)) do (incf i))
               (setf i (min length (1+ i))))
             (skip-block-comment ()
               (let ((depth 0))
                 (loop while (< i length)
                       do (cond ((and (eql (peek) #\#) (eql (peek 1) #\|))
                                 (incf depth) (incf i 2))
                                ((and (eql (peek) #\|) (eql (peek 1) #\#))
                                 (decf depth) (incf i 2)
                                 (when (zerop depth) (return)))
                                (t (incf i))))))
             (skip-token ()
               (loop while (< i length)
                     do (let ((character (char code i)))
                          (cond ((or (whitespace-p character)
                                     (find character "()\";'`,"))
                                 (return))
                                ((char= character #\|) (skip-pipe))
                                ((char= character #\\) (incf i 2))
                                (t (incf i)))))
               (setf i (min i length)))
             (skip-form ()
               (let ((character (peek)))
                 (cond
                   ((null character))
                   ((find character "'`") (incf i) (skip-form))
                   ((char= character #\,)
                    (incf i (if (eql (peek 1) #\@) 2 1)) (skip-form))
                   ((and (char= character #\#) (member (peek 1) '(#\' #\( #\+ #\-)))
                    (incf i (if (eql (peek 1) #\() 1 2))
                    (if (member (char code (1- i)) '(#\+ #\-))
                        (progn (skip-form) (skip-blank) (skip-form))
                        (skip-form)))
                   ((and (char= character #\#) (eql (peek 1) #\\))
                    (incf i 3) (skip-token))
                   ((char= character #\") (skip-string))
                   ((char= character #\()
                    (incf i)
                    (loop
                      (skip-blank)
                      (cond ((>= i length) (return))
                            ((char= (char code i) #\)) (incf i) (return))
                            (t (let ((before i))
                                 (skip-form)
                                 ;; A stray character must not stall the scan.
                                 (when (= i before) (incf i)))))))
                   (t (skip-token)))))
             (skip-blank ()
               (loop while (< i length)
                     do (cond ((whitespace-p (char code i)) (incf i))
                              ((char= (char code i) #\;)
                               (loop while (and (< i length) (char/= (char code i) #\Newline))
                                     do (incf i)))
                              ((and (eql (peek) #\#) (eql (peek 1) #\|))
                               (skip-block-comment))
                              (t (return))))))
      (skip-blank)
      (let ((form-start i))
        (skip-form)
        (values (min i length) form-start)))))

(defun code-summary (code)
  "CODE's first form on one line, with an ellipsis when more forms follow."
  (multiple-value-bind (end start) (lisp-form-end code 0)
    (let* ((blank '(#\Space #\Tab #\Newline #\Return))
           ;; A stray character such as ")" is no form: show everything.
           (form (if (> end start) (subseq code start end) code))
           (more (and (> end start)
                      (> (lisp-form-end code end) (nth-value 1 (lisp-form-end code end))))))
      (format nil "~{~A~^ ~}~:[~; …~]"
              (remove "" (uiop:split-string form :separator blank) :test #'string=)
              more))))

(defun tool-bullet (status)
  (styled (ecase status (:ok :tool-ok) (:error :tool-error) (:pending :dim)) "•"))

(defun tool-summary-rows (code result status width)
  "The summary block: a header with the first form, then the result's last
lines, dimmed, and a count of the lines left out."
  (let* ((limit (or width 100))
         (header (format nil "~A ~A" (tool-bullet status) (styled :tool-header "lisp")))
         (rows (list (if code
                         (concatenate 'string header " "
                                      (highlight-code (clip-line (code-summary code)
                                                                 (max 1 (- limit 7)))
                                                      :lisp))
                         header)))
         (lines (and result (text-lines result)))
         (shown (last lines 3))
         (hidden (+ (- (length lines) (length shown))
                    (if code (max 0 (1- (length (text-lines code)))) 0))))
    (loop for line in shown
          for prefix = "  └ " then "    "
          do (push (concatenate 'string prefix
                                (styled :dim (clip-line line (max 1 (- limit 4)))))
                   rows))
    (when (plusp hidden)
      (push (styled :dim (format nil "    … +~D line~:P (ctrl-t to expand)" hidden)) rows))
    (nreverse rows)))

(defun tool-full-rows (code result status width)
  "The full block: a header, all of the code highlighted as Lisp, then the
whole result, highlighted as Lisp unless it is an error."
  (append
    (list (format nil "~A ~A" (tool-bullet status) (styled :tool-header "lisp")))
    (and code
         (indent-rows (highlight-code (string-right-trim '(#\Space #\Tab #\Newline) code) :lisp)
                      width "    " "    "))
    (and result
         (indent-rows (let ((text (format nil "~{~A~^~%~}" (or (text-lines result) '("")))))
                        (if (eq status :error)
                            (style-lines :error text)
                            (highlight-code text :lisp)))
                      width "  └ " "    "))))

(defun paired-echo-p (entries index)
  "True when entry INDEX is a result drawn as part of the tool call before it."
  (and (plusp index)
       (eq (entry-kind (aref entries index)) :echo)
       (eq (entry-kind (aref entries (1- index))) :tool)))

(defun render-entry-rows (entries index width mode)
  "Render entry INDEX of ENTRIES as a list of rows, without the cache. A tool
call and the result after it are drawn together, at the call."
  (incf *entry-render-count*)
  (let* ((entry (aref entries index))
         (text (entry-text entry))
         (rows
           (case (entry-kind entry)
             ((:tool :echo)
              (unless (paired-echo-p entries index)
                (let* ((tool-p (eq (entry-kind entry) :tool))
                       (echo (if tool-p
                                 (and (< (1+ index) (length entries))
                                      (eq (entry-kind (aref entries (1+ index))) :echo)
                                      (aref entries (1+ index)))
                                 entry))
                       (status (cond ((null echo) :pending)
                                     ((entry-ok-p echo) :ok)
                                     (t :error))))
                  (funcall (if (eq mode :full) #'tool-full-rows #'tool-summary-rows)
                           (and tool-p text) (and echo (entry-text echo)) status width))))
             (:user (indent-rows (style-lines :user text) width
                                 (format nil "~A " (styled :user-gutter "›")) "  "))
             (:assistant (indent-rows (render-markdown text) width "" ""))
             (:steer (indent-rows (style-lines :steer text) width
                                  (format nil "~A " (styled :steer "steer>")) "       "))
             (:status (indent-rows (style-lines (if (entry-ok-p entry) :dim :error) text)
                                   width "" ""))
             (t (indent-rows text width "" "")))))
    ;; On a terminal, a blank row separates entries.
    (coerce (if (and rows width) (append rows (list "")) rows) 'vector)))

(defun entry-rows (entries index width &optional (mode *tool-display*))
  "The rows of entry INDEX of ENTRIES at WIDTH in MODE, from the cache when
they have been rendered before."
  (unless (eql width *row-cache-width*)
    (clear-row-cache)
    (setf *row-cache-width* width))
  (let* ((entry (aref entries index))
         (key (list mode width *color*))
         (cached (assoc key (gethash entry *row-cache*) :test #'equal)))
    (if cached
        (cdr cached)
        (let ((rows (render-entry-rows entries index width mode)))
          (push (cons key rows) (gethash entry *row-cache*))
          rows))))

;;; Recording and live printing.

(defun write-rows (rows stream)
  (fresh-line stream)
  (loop for row across rows do (write-line row stream))
  (finish-output stream))

(defun show-entry (entries index &optional (stream *standard-output*))
  "Print entry INDEX of ENTRIES as it arrives. A tool call waits for its
result, and the pair is printed then."
  (let ((entry (aref entries index)))
    (unless (eq (entry-kind entry) :tool)
      ;; The steering line must go away before the transcript grows.
      (when *input-hide* (funcall *input-hide*))
      (write-rows (entry-rows entries (if (paired-echo-p entries index) (1- index) index)
                              (live-width stream))
                  stream))))

(defun print-entry (entry &optional (stream *standard-output*))
  "Print ENTRY on its own, without recording it."
  (show-entry (vector entry) 0 stream))

(defun record-entry (kind text &key (ok-p t) id (show *show-entries*)
                                    (stream *standard-output*))
  "Append an entry to *SESSION-ENTRIES* and return it. When SHOW is true it is
printed on STREAM. The caller may set its ENTRY-ID once it is logged."
  (let ((entry (make-entry kind text :ok-p ok-p :id id))
        (entries *session-entries*))
    ;; A result changes how the tool call before it is drawn.
    (when (and (eq kind :echo) (plusp (length entries)))
      (remhash (aref entries (1- (length entries))) *row-cache*))
    (vector-push-extend entry entries)
    (when show (show-entry entries (1- (length entries)) stream))
    entry))

(defun log-message-entry (message)
  "A transcript entry for MESSAGE from the OptChat log."
  (make-entry (case (message-kind message)
                (:maxwell :assistant)
                (:note :status)
                (:work :assistant)
                (t (message-kind message)))
              (message-text message)
              :id (message-id message)
              :time (message-time message)))

(defun show-recent-exchanges (entries &key (count 3) (stream *standard-output*))
  "Print the last COUNT exchanges of ENTRIES, each starting at a :user entry,
dimmed and with tool calls summarized, so a resumed chat shows where it left
off. ENTRIES is a vector, for example log messages made into entries with
LOG-MESSAGE-ENTRY."
  (let ((start (or (loop with seen = 0
                         for index from (1- (length entries)) downto 0
                         when (eq (entry-kind (aref entries index)) :user)
                           do (incf seen)
                              (when (= seen count) (return index)))
                   0))
        (width (live-width stream)))
    (fresh-line stream)
    (loop for index from start below (length entries)
          do (loop for row across (entry-rows entries index width :summary)
                   do (write-line (styled :dim (strip-ansi row)) stream)))
    (finish-output stream)))

;;; The lazy viewer. It renders a contiguous range of entries, [LO, HI), into
;;; rows held in two stacks: rows prepended above (ABOVE, nearest first) and
;;; rows appended below (BELOW, in order), so either end grows cheaply. More
;;; entries are rendered only when the screen comes within a page of an end,
;;; and START moves with prepended rows so the screen does not jump.

(defstruct (viewer (:constructor %make-viewer))
  entries
  (lo 0) (hi 0)
  (above (make-entry-vector)) (above-owners (make-entry-vector))
  (below (make-entry-vector)) (below-owners (make-entry-vector))
  (start 0)
  (width 79)
  (page-size 22)
  exhausted)

(defun viewer-row-count (viewer)
  (+ (length (viewer-above viewer)) (length (viewer-below viewer))))

(defun viewer-row (viewer k)
  "Row K counted from the top of the rendered rows, and the entry it belongs to."
  (let ((above (length (viewer-above viewer))))
    (if (< k above)
        (values (aref (viewer-above viewer) (- above k 1))
                (aref (viewer-above-owners viewer) (- above k 1)))
        (values (aref (viewer-below viewer) (- k above))
                (aref (viewer-below-owners viewer) (- k above))))))

(defun viewer-load-older (viewer)
  "Prepend older entries from *OLDER-ENTRIES-FUNCTION*. NIL when none remain."
  (let* ((entries (viewer-entries viewer))
         (older (and (not (viewer-exhausted viewer))
                     *older-entries-function*
                     (funcall *older-entries-function*
                              (and (plusp (length entries)) (aref entries 0))
                              *older-batch-size*))))
    (if (zerop (length older))
        (progn (setf (viewer-exhausted viewer) t) nil)
        (let ((count (length older)))
          (setf (viewer-entries viewer) (concatenate 'vector older entries))
          (incf (viewer-lo viewer) count)
          (incf (viewer-hi viewer) count)
          t))))

(defun viewer-render-above (viewer min-rows)
  "Render older entries above the rendered rows until at least MIN-ROWS rows
were added or none remain, moving START so the same rows stay on screen."
  (let ((added 0)
        (width (viewer-width viewer)))
    (loop while (< added min-rows)
          do ;; Load before rendering the oldest loaded entry, whose drawing
             ;; depends on the entry before it.
             (when (and (<= (viewer-lo viewer) 1) (not (viewer-exhausted viewer)))
               (viewer-load-older viewer))
             (when (zerop (viewer-lo viewer)) (return))
             (let* ((index (decf (viewer-lo viewer)))
                    (entry (aref (viewer-entries viewer) index))
                    (rows (entry-rows (viewer-entries viewer) index width)))
               (loop for k from (1- (length rows)) downto 0
                     do (vector-push-extend (aref rows k) (viewer-above viewer))
                        (vector-push-extend entry (viewer-above-owners viewer)))
               (incf added (length rows))))
    (incf (viewer-start viewer) added)
    added))

(defun viewer-render-below (viewer min-rows)
  "Render newer entries below the rendered rows until at least MIN-ROWS rows
were added or none remain."
  (let ((added 0)
        (entries (viewer-entries viewer)))
    (loop while (and (< added min-rows) (< (viewer-hi viewer) (length entries)))
          do (let* ((index (viewer-hi viewer))
                    (entry (aref entries index))
                    (rows (entry-rows entries index (viewer-width viewer))))
               (loop for row across rows
                     do (vector-push-extend row (viewer-below viewer))
                        (vector-push-extend entry (viewer-below-owners viewer)))
               (incf (viewer-hi viewer))
               (incf added (length rows))))
    added))

(defun viewer-more-above-p (viewer)
  "True when older entries may exist above the rendered rows."
  (or (plusp (viewer-lo viewer)) (not (viewer-exhausted viewer))))

(defun viewer-settle (viewer)
  "Render more when the screen is within a page of either end of the rendered
rows, then keep START within them."
  (let ((page (viewer-page-size viewer)))
    (when (< (viewer-start viewer) page)
      (viewer-render-above viewer page))
    (let ((needed (- (+ (viewer-start viewer) (* 2 page)) (viewer-row-count viewer))))
      (when (plusp needed) (viewer-render-below viewer needed)))
    (setf (viewer-start viewer)
          (max 0 (min (viewer-start viewer) (- (viewer-row-count viewer) page))))
    viewer))

(defun open-viewer (entries width page-size &key (fill (* 2 page-size)))
  "A viewer over ENTRIES with at least FILL of the newest rows rendered (all of
them when there are fewer), showing the page above the last."
  (let ((viewer (%make-viewer :entries entries :width width :page-size page-size
                              :lo (length entries) :hi (length entries))))
    (viewer-render-above viewer fill)
    (setf (viewer-start viewer)
          (max 0 (- (viewer-row-count viewer) (* 2 page-size))))
    (viewer-settle viewer)))

(defun viewer-scroll (viewer pages)
  "Move the screen by PAGES pages, rendering older or newer entries as needed."
  (setf (viewer-start viewer)
        (max 0 (+ (viewer-start viewer) (* pages (viewer-page-size viewer)))))
  (viewer-settle viewer))

(defun viewer-at-bottom-p (viewer)
  (and (= (viewer-hi viewer) (length (viewer-entries viewer)))
       (>= (viewer-start viewer)
           (- (viewer-row-count viewer) (viewer-page-size viewer)))))

(defun viewer-top-entry (viewer)
  "The entry at the top of the screen and how many of its rows are above it."
  (let ((start (viewer-start viewer)))
    (if (>= start (viewer-row-count viewer))
        (values nil 0)
        (let ((owner (nth-value 1 (viewer-row viewer start)))
              (first start))
          (loop while (and (plusp first)
                           (eq owner (nth-value 1 (viewer-row viewer (1- first)))))
                do (decf first))
          (values owner (- start first))))))

(defun viewer-rerender (viewer &key (width (viewer-width viewer))
                                    (page-size (viewer-page-size viewer)))
  "Drop the rendered rows and render again around the entry at the top of the
screen, which stays at the top. Used after Ctrl-T and when the terminal is
resized; older rows are rendered again lazily."
  (multiple-value-bind (anchor offset) (viewer-top-entry viewer)
    (let ((index (or (and anchor (position anchor (viewer-entries viewer)))
                     (length (viewer-entries viewer)))))
      (dolist (stack (list (viewer-above viewer) (viewer-above-owners viewer)
                           (viewer-below viewer) (viewer-below-owners viewer)))
        (setf (fill-pointer stack) 0))
      (setf (viewer-width viewer) width
            (viewer-page-size viewer) page-size
            (viewer-lo viewer) index
            (viewer-hi viewer) index
            (viewer-start viewer) 0)
      (viewer-render-below viewer (+ offset (* 2 page-size)))
      (setf (viewer-start viewer)
            (min offset (max 0 (1- (viewer-row-count viewer)))))
      (viewer-settle viewer))))

(defun transcript-tail (entries width rows)
  "The newest rows of ENTRIES, at least 2 × ROWS of them when there are that
many, and whether older history remains. Only the entries needed are
rendered, so the cost depends on ROWS, not on the length of ENTRIES."
  (let ((viewer (%make-viewer :entries entries :width width :page-size rows
                              :lo (length entries) :hi (length entries))))
    (viewer-render-above viewer (* 2 rows))
    (values (loop for k below (viewer-row-count viewer)
                  collect (viewer-row viewer k))
            (viewer-more-above-p viewer))))

(defun redraw-transcript (&optional (output *standard-output*))
  "Clear the screen and its scrollback and draw the newest entries again in the
current tool display mode, about two pages of them. The caller redraws its
prompt line afterwards."
  (multiple-value-bind (rows columns) (terminal-size)
    (multiple-value-bind (lines more) (transcript-tail *session-entries* (1- columns) rows)
      (format output "~C[H~C[2J~C[3J" #\Escape #\Escape #\Escape)
      (when more
        (write-line (styled :dim "… earlier history: PgUp") output))
      (dolist (line lines) (write-line line output))
      (finish-output output))))
