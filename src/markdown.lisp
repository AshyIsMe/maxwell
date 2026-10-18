(in-package #:maxwell)

;;; Light markdown rendering for assistant replies. Headings, emphasis, inline
;;; code, bullets, and fenced code are styled with their markers hidden;
;;; tables, links, and quotes pass through. With colour off the text is
;;; returned unchanged, so --print output stays faithful.

(defun styled-within (face string base)
  "STYLED, then reopen the BASE face so an enclosing span continues."
  (let ((result (styled face string))
        (base-code (and base (face-code base))))
    (if (and *color* base-code (not (eq result string)))
        (concatenate 'string result (sgr base-code))
        result)))

(defun word-char-p (character)
  (and character (alphanumericp character)))

(defun char-at (text index)
  (and (< -1 index (length text)) (char text index)))

(defun find-emphasis-close (text start marker)
  "Index of a closing MARKER for emphasis opened just before START, or NIL.
The closer must follow a non-blank and must not be followed by a word
character, so markers inside words such as snake_case_name never match."
  (let ((width (length marker)))
    (loop with i = start
          while (< i (length text))
          do (cond ((char= (char text i) #\`)
                    ;; Markers inside code spans do not close emphasis.
                    (let ((close (position #\` text :start (1+ i))))
                      (setf i (if close (1+ close) (1+ i)))))
                   ((and (prefix-at-p marker text i)
                         (> i start)
                         (not (whitespace-p (char text (1- i))))
                         (not (word-char-p (char-at text (+ i width))))
                         ;; A single * must not be half of a **.
                         (or (= width 2) (not (eql (char-at text (1+ i)) (char marker 0)))))
                    (return i))
                   ((and (= width 1) (prefix-at-p (make-string 2 :initial-element (char marker 0)) text i))
                    (incf i 2))
                   (t (incf i))))))

(defun render-inline (text &optional base)
  "Style inline markdown in TEXT: code spans, bold, italic, and dim links.
BASE is the face of an enclosing span, reopened after each inner one."
  (with-output-to-string (out)
    (let ((n (length text)) (i 0))
      (loop while (< i n)
            do (let* ((character (char text i))
                      (previous (char-at text (1- i)))
                      (double (and (find character "*_") (eql (char-at text (1+ i)) character)))
                      (marker (cond (double (make-string 2 :initial-element character))
                                    ((find character "*_") (string character)))))
                 (cond
                   ((char= character #\\)
                    (write-string text out :start i :end (min n (+ i 2)))
                    (incf i 2))
                   ((char= character #\`)
                    (let* ((ticks (- (scan-while text i (lambda (c) (char= c #\`))) i))
                           (fence (make-string ticks :initial-element #\`))
                           (close (search fence text :start2 (+ i ticks))))
                      (if close
                          (let ((code (subseq text (+ i ticks) close)))
                            (when (and (> (length code) 1) (char= (char code 0) #\Space)
                                       (char= (char code (1- (length code))) #\Space))
                              (setf code (subseq code 1 (1- (length code)))))
                            (write-string (styled-within :code-inline code base) out)
                            (setf i (+ close ticks)))
                          (progn (write-string fence out) (incf i ticks)))))
                   ((and marker
                         (not (word-char-p previous))
                         (let ((after (char-at text (+ i (length marker)))))
                           (and after (not (whitespace-p after))))
                         (find-emphasis-close text (+ i (length marker)) marker))
                    (let* ((start (+ i (length marker)))
                           (close (find-emphasis-close text start marker))
                           (face (if double :bold :italic)))
                      (write-string (styled-within face (render-inline (subseq text start close) face)
                                                   base)
                                    out)
                      (setf i (+ close (length marker)))))
                   ((and (char= character #\[) (link-end text i))
                    (let ((end (link-end text i)))
                      (write-string (styled-within :dim (subseq text i end) base) out)
                      (setf i end)))
                   ((and (not (word-char-p previous))
                         (or (prefix-at-p "http://" text i) (prefix-at-p "https://" text i)))
                    (let ((end (or (position-if #'whitespace-p text :start i) n)))
                      (write-string (styled-within :dim (subseq text i end) base) out)
                      (setf i end)))
                   (marker (write-string marker out) (incf i (length marker)))
                   (t (write-char character out) (incf i))))))))

(defun link-end (text start)
  "If a [text](url) link starts at START, the index just past it."
  (let ((close (position #\] text :start start)))
    (when (and close (eql (char-at text (1+ close)) #\())
      (let ((paren (position #\) text :start (1+ close))))
        (and paren (1+ paren))))))

(defun fence-marker (line)
  "The ``` or ~~~ run opening or closing a fence on LINE, and the info string."
  (let* ((indent (scan-while line 0 #'blank-p))
         (character (char-at line indent)))
    (when (and (<= indent 3) (member character '(#\` #\~)))
      (let ((end (scan-while line indent (lambda (c) (char= c character)))))
        (when (>= (- end indent) 3)
          (values (subseq line indent end) (string-trim " " (subseq line end))))))))

(defun list-item-parts (line)
  "For a list item, return its indentation, marker, and text."
  (let* ((indent (scan-while line 0 #'blank-p))
         (character (char-at line indent)))
    (cond ((and character (find character "-*+") (eql (char-at line (1+ indent)) #\Space))
           (values (subseq line 0 indent) (string character) (subseq line (+ indent 2))))
          ((and character (digit-char-p character))
           (let ((end (scan-while line indent #'digit-char-p)))
             (when (and (member (char-at line end) '(#\. #\))) (eql (char-at line (1+ end)) #\Space))
               (values (subseq line 0 indent) (subseq line indent (1+ end))
                       (subseq line (+ end 2)))))))))

(defun render-markdown-line (line)
  (let ((indent (scan-while line 0 #'blank-p)))
    (cond
      ((and (<= indent 3) (eql (char-at line indent) #\#))
       (let ((hashes (- (scan-while line indent (lambda (c) (char= c #\#))) indent)))
         (if (and (<= hashes 6) (or (= (+ indent hashes) (length line))
                                    (blank-p (char line (+ indent hashes)))))
             (let ((title (string-trim " #" (subseq line (+ indent hashes)))))
               (styled :heading (render-inline title :heading)))
             (render-inline line))))
      ((eql (char-at line indent) #\|) line)
      (t (multiple-value-bind (prefix marker body) (list-item-parts line)
           (if marker
               (format nil "~A~A ~A" prefix
                       (styled :dim (if (digit-char-p (char marker 0)) marker "•"))
                       (render-inline body))
               (render-inline line)))))))

(defun render-code-block (lines info)
  (when lines
   (let* ((code (format nil "~{~A~^~%~}" lines))
         (language (detect-language code info)))
    (mapcar (lambda (line) (concatenate 'string "  " line))
            (uiop:split-string (if language (highlight-code code language) code)
                               :separator '(#\Newline))))))

(defun render-markdown (text)
  "Render light markdown in TEXT for the terminal. Fence lines are dropped and
their code is highlighted and indented by two spaces. With colour off, TEXT is
returned unchanged."
  (if (not *color*)
      text
      (let ((output '()) (fence nil) (info nil) (code '()))
        (dolist (line (uiop:split-string text :separator '(#\Newline)))
          (multiple-value-bind (marker marker-info) (fence-marker line)
            (cond
              ((and fence marker (char= (char marker 0) (char fence 0))
                    (>= (length marker) (length fence)) (zerop (length marker-info)))
               (setf output (revappend (render-code-block (nreverse code) info) output)
                     fence nil code '()))
              (fence (push line code))
              (marker (setf fence marker info marker-info))
              (t (push (render-markdown-line line) output)))))
        ;; An unterminated fence still renders as code.
        (when fence
          (setf output (revappend (render-code-block (nreverse code) info) output)))
        (format nil "~{~A~^~%~}" (nreverse output)))))
