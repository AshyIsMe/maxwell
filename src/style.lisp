(in-package #:maxwell)

;;; Terminal colour: faces, SGR codes, and ANSI-aware measuring and wrapping.
;;; Styling is applied only when rendering for the terminal; styled text must
;;; never reach the model or the log.

(defvar *color* nil
  "True when output should carry SGR colour codes. Recomputed at startup and
after an image resumes with CONFIGURE-COLOR; never meaningful when saved.")

(defparameter *faces*
  '(;; Transcript
    (:user . "1")
    (:user-gutter . "36")
    (:assistant . nil)
    (:tool-header . "1")
    (:tool-ok . "32")
    (:tool-error . "31")
    (:dim . "2")
    (:error . "31")
    (:steer . "33")
    ;; Markdown
    (:heading . "1;35")
    (:code-inline . "36")
    (:bold . "1")
    (:italic . "3")
    ;; Syntax
    (:kw . "35")
    (:str . "32")
    (:comment . "2")
    (:num . "33")
    (:sym . "36")
    (:fn . "34")
    (:type . "1;36")
    (:const . "33")
    (:punct . nil)
    (:added . "32")
    (:removed . "31")
    (:hunk . "36"))
  "Face name -> SGR parameter string (NIL for the terminal default). Only the 16
standard ANSI colours plus bold, dim, and italic, so the user's theme decides
the actual shades. Change the palette here.")

(defun face-code (face)
  "The SGR parameter string for FACE, or NIL."
  (cdr (assoc face *faces*)))

(defun detect-color (&optional (mode :auto))
  "Decide whether to use colour for MODE (:always, :never, or :auto).
:auto means stdout is interactive, TERM is not dumb, and NO_COLOR is unset."
  (ecase mode
    (:always t)
    (:never nil)
    (:auto (and (interactive-stream-p *standard-output*)
                (not (equal (uiop:getenv "TERM") "dumb"))
                (zerop (length (uiop:getenv "NO_COLOR")))
                t))))

(defun parse-color-mode (mode)
  (cond ((member mode '(:auto :always :never)) mode)
        ((and (stringp mode) (find (string-downcase mode) '("auto" "always" "never")
                                   :test #'string=))
         (intern (string-upcase mode) :keyword))
        (t (error "Invalid colour mode ~S; expected auto, always, or never." mode))))

(defun configure-color (&optional (mode :auto))
  "Set *COLOR* from MODE, a keyword or one of the strings auto, always, never."
  (setf *color* (detect-color (parse-color-mode mode))))

(defun sgr (code)
  (format nil "~C[~Am" #\Escape code))

(defun styled (face string)
  "Return STRING wrapped in FACE's SGR codes, or STRING itself when colour is
off, FACE has no code, or STRING is empty."
  (let ((code (face-code face)))
    (if (and *color* code (plusp (length string)))
        (concatenate 'string (sgr code) string (sgr "0"))
        string)))

(defmacro with-face ((face stream) &body body)
  "Run BODY with FACE's SGR state active on STREAM when colour is on."
  (let ((code (gensym "CODE")) (out (gensym "STREAM")))
    `(let* ((,out ,stream)
            (,code (and *color* (face-code ,face))))
       (when ,code (write-string (sgr ,code) ,out))
       (unwind-protect (progn ,@body)
         (when ,code (write-string (sgr "0") ,out))))))

;;; Escape sequences. CSI is ESC [ params final, with the final byte in
;;; @..~; OSC is ESC ] ... terminated by BEL or ESC \. Anything else after
;;; ESC is treated as a two-character sequence.

(defun escape-end (string start)
  "STRING has ESC at START. Return the index just past the sequence and, for an
SGR sequence, its parameter string as a second value."
  (let ((length (length string))
        (next (1+ start)))
    (cond
      ((>= next length) length)
      ((char= (char string next) #\[)
       (let ((final (position-if (lambda (c) (char<= #\@ c #\~)) string :start (1+ next))))
         (if final
             (values (1+ final)
                     (and (char= (char string final) #\m)
                          (subseq string (1+ next) final)))
             length)))
      ((char= (char string next) #\])
       (loop for i from (1+ next) below length
             do (cond ((char= (char string i) (code-char 7)) (return (1+ i)))
                      ((and (char= (char string i) #\Escape)
                            (< (1+ i) length)
                            (char= (char string (1+ i)) #\\))
                       (return (+ i 2))))
             finally (return length)))
      (t (1+ next)))))

(defun printable-char-p (character)
  (let ((code (char-code character)))
    (and (>= code 32) (/= code 127) (not (<= 128 code 159)))))

(defun strip-ansi (string)
  "Return STRING without terminal escape sequences."
  (with-output-to-string (out)
    (loop with i = 0
          while (< i (length string))
          do (if (char= (char string i) #\Escape)
                 (setf i (escape-end string i))
                 (progn (write-char (char string i) out) (incf i))))))

(defun visible-length (string)
  "Number of printable characters in STRING, ignoring escape sequences and
other control characters."
  (count-if #'printable-char-p (strip-ansi string)))

(defun update-sgr-state (state parameters)
  "Return the active SGR parameter list after applying PARAMETERS to STATE."
  (let ((parameters (uiop:split-string parameters :separator ";")))
    (loop while parameters
          do (let ((parameter (pop parameters)))
               (cond ((member parameter '("" "0") :test #'string=)
                      (setf state '()))
                     ;; Extended colours carry sub-parameters (38;5;N or
                     ;; 38;2;R;G;B) whose zeros are not resets.
                     ((member parameter '("38" "48" "58") :test #'string=)
                      (let ((count (if (equal (first parameters) "2") 4 2)))
                        (setf state
                              (append state
                                      (list (format nil "~A~{;~A~}" parameter
                                                    (subseq parameters 0 (min count (length parameters)))))))
                        (setf parameters (nthcdr count parameters))))
                     (t (setf state (append state (list parameter)))))))
    state))

(defun ansi-wrap (text width)
  "Wrap TEXT into a vector of lines at most WIDTH visible columns wide.
SGR sequences are kept and do not count toward the width; at each line break
the active SGR state is closed with ESC[0m and reopened on the next line, so
every line can be printed on its own. Tabs expand to 8-column stops; other
escape sequences and control characters are dropped."
  (let ((width (max 1 width))
        (lines '())
        (line (make-string-output-stream))
        (column 0)
        (state '()))
    (labels ((flush-line ()
               (when state (write-string (sgr "0") line))
               (push (get-output-stream-string line) lines)
               (setf column 0)
               (when state
                 (write-string (sgr (format nil "~{~A~^;~}" state)) line)))
             (emit (character)
               (when (= column width) (flush-line))
               (write-char character line)
               (incf column)))
      (loop with i = 0
            while (< i (length text))
            do (let ((character (char text i)))
                 (cond
                   ((char= character #\Escape)
                    (multiple-value-bind (end parameters) (escape-end text i)
                      (when parameters
                        (write-string text line :start i :end end)
                        (setf state (update-sgr-state state parameters)))
                      (setf i end)))
                   (t
                    (cond ((char= character #\Newline) (flush-line))
                          ((char= character #\Tab)
                           (dotimes (k (- 8 (mod column 8))) (emit #\Space)))
                          ((printable-char-p character) (emit character)))
                    (incf i)))))
      (when (or (plusp column) (null lines))
        (when state (write-string (sgr "0") line))
        (push (get-output-stream-string line) lines)))
    (coerce (nreverse lines) 'vector)))
