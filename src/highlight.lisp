(in-package #:maxwell)

;;; Hand-written syntax highlighters, Common Lisp first.
;;;
;;; Every lexer turns a string into a list of (FACE . TEXT) tokens whose texts
;;; concatenate back to the input; FACE is NIL for plain text. HIGHLIGHT-CODE
;;; only styles those tokens, so the lexers can be tested without colour.
;;; Lexers must never signal: unterminated strings and comments simply run to
;;; the end of the input.

(defparameter *rainbow-parens* nil
  "When true, Lisp parentheses at odd depths are dimmed.")

(defvar *tokens* '()
  "Tokens being collected by the running lexer, most recent first.")

(defun emit (face text start end)
  (when (< start (min end (length text)))
    (push (cons face (subseq text start (min end (length text)))) *tokens*)))

(defun whitespace-p (character)
  (member character '(#\Space #\Tab #\Newline #\Return #\Page)))

(defun blank-p (character)
  (member character '(#\Space #\Tab)))

(defun scan-while (text start predicate)
  (or (position-if-not predicate text :start (min start (length text))) (length text)))

(defun line-end (text start)
  (or (position #\Newline text :start (min start (length text))) (length text)))

(defun next-nonblank (text start)
  (let ((index (scan-while text start #'blank-p)))
    (and (< index (length text)) (char text index))))

(defun prefix-at-p (prefix text start)
  (let ((end (+ start (length prefix))))
    (and (<= end (length text)) (string= prefix text :start2 start :end2 end))))

(defun line-start-p (text index)
  "True when only blanks precede INDEX on its line."
  (loop for i downfrom (1- index) to 0
        for character = (char text i)
        do (cond ((char= character #\Newline) (return t))
                 ((not (blank-p character)) (return nil)))
        finally (return t)))

(defun scan-quoted (text start close &key (escapes t))
  "Index just past the string opened at START and closed by the string CLOSE."
  (let ((n (length text)))
    (loop with i = start
          while (< i n)
          do (cond ((and escapes (char= (char text i) #\\)) (incf i 2))
                   ((prefix-at-p close text i) (return (+ i (length close))))
                   (t (incf i)))
          finally (return n))))

(defun scan-block-comment (text start open close &key nested)
  (let ((n (length text)) (depth 0))
    (loop with i = start
          while (< i n)
          do (cond ((prefix-at-p open text i) (incf depth) (incf i (length open)))
                   ((prefix-at-p close text i)
                    (incf i (length close))
                    (when (or (not nested) (zerop (decf depth))) (return i)))
                   (t (incf i)))
          finally (return n))))

(defun scan-number (text start)
  "Index past a C-style number: digits, letters (hex, suffixes), underscores,
a decimal point before a digit, and an exponent sign."
  (let* ((n (length text))
         (hex (and (< (1+ start) n) (char= (char text start) #\0)
                   (find (char text (1+ start)) "xX"))))
    (loop with i = start
          while (< i n)
          do (let ((character (char text i)))
               (cond ((or (alphanumericp character) (char= character #\_)) (incf i))
                     ((and (char= character #\.) (< (1+ i) n) (digit-char-p (char text (1+ i))))
                      (incf i))
                     ((and (find character "+-") (not hex) (> i start)
                           (find (char text (1- i)) "eE"))
                      (incf i))
                     (t (return i))))
          finally (return n))))

;;; Common Lisp.

(defun lisp-terminator-p (character)
  (or (whitespace-p character) (find character "()'\";`,[]{}")))

(defun scan-lisp-token (text start)
  "Index past the symbol or number token at START, honouring |...| and \\."
  (let ((n (length text)))
    (loop with i = start
          while (< i n)
          do (let ((character (char text i)))
               (cond ((char= character #\\) (incf i 2))
                     ((char= character #\|)
                      (setf i (let ((close (position #\| text :start (1+ i))))
                                (if close (1+ close) n))))
                     ((lisp-terminator-p character) (return i))
                     (t (incf i))))
          finally (return n))))

(defun lisp-number-p (token)
  "True when TOKEN reads as a decimal integer, ratio, or float."
  (let ((i 0) (n (length token)))
    (labels ((peek () (and (< i n) (char token i)))
             (digits () (let ((start i))
                          (loop while (and (peek) (digit-char-p (peek))) do (incf i))
                          (> i start))))
      (when (and (peek) (find (peek) "+-")) (incf i))
      (let ((integer (digits)))
        (cond ((and integer (eql (peek) #\/))
               (incf i)
               (and (digits) (= i n)))
              (t
               (let ((fraction (and (eql (peek) #\.) (progn (incf i) (digits)))))
                 (and (or integer fraction)
                      (or (= i n)
                          (and (find (peek) "esfdlESFDL")
                               (progn (incf i)
                                      (when (and (peek) (find (peek) "+-")) (incf i))
                                      (digits))
                               (= i n)))))))))))

(defun cl-symbol-face (token)
  "Face for TOKEN taken from the live image's COMMON-LISP package."
  (let* ((colon (position #\: token :from-end t))
         (prefix (and colon (string-upcase (string-right-trim ":" (subseq token 0 colon)))))
         (name (if colon (subseq token (1+ colon)) token)))
    (when (or (null prefix) (member prefix '("CL" "COMMON-LISP") :test #'string=))
      (multiple-value-bind (symbol status) (find-symbol (string-upcase name) :cl)
        (when status
          (cond ((or (special-operator-p symbol) (macro-function symbol)) :kw)
                ((and (boundp symbol) (constantp symbol)) :const)
                ((fboundp symbol) :fn)))))))

(defun lisp-token-face (token)
  (cond ((char= (char token 0) #\:) :sym)
        ((char= (char token 0) #\&) :kw)
        ((lisp-number-p token) :num)
        ((find #\| token) nil)
        (t (cl-symbol-face token))))

(defun lisp-definer-face (token face)
  "When TOKEN names a def* form, the face for the name it defines."
  (let ((name (string-upcase token)))
    (when (or (and (eq face :kw) (prefix-at-p "DEF" name 0))
              (prefix-at-p "DEFINE-" name 0)
              (string= name "DEFSYSTEM"))
      (if (member name '("DEFCLASS" "DEFSTRUCT" "DEFTYPE" "DEFINE-CONDITION" "DEFPACKAGE")
                  :test #'string=)
          :type
          :fn))))

(defun paren-face (depth)
  (if (and *rainbow-parens* (oddp depth)) :dim :punct))

(defun lex-lisp (text)
  (let ((n (length text)) (i 0) (depth 0) (after-open nil) (pending nil))
    (loop while (< i n)
          do (let* ((start i)
                    (character (char text i))
                    (next (and (< (1+ i) n) (char text (1+ i)))))
               (flet ((tok (face end &optional (keep-state nil))
                        (emit face text start end)
                        (setf i (min n end))
                        (unless keep-state (setf after-open nil pending nil))))
                 (cond
                   ((whitespace-p character) (tok nil (scan-while text i #'whitespace-p) t))
                   ((char= character #\;) (tok :comment (line-end text i) t))
                   ((and (char= character #\#) (eql next #\|))
                    (tok :comment (scan-block-comment text i "#|" "|#" :nested t) t))
                   ((char= character #\") (tok :str (scan-quoted text (1+ i) "\"")))
                   ((find character "([{")
                    (tok (paren-face depth) (1+ i))
                    (incf depth)
                    (setf after-open t))
                   ((find character ")]}")
                    (setf depth (max 0 (1- depth)))
                    (tok (paren-face depth) (1+ i)))
                   ((find character "'`")
                    (tok :punct (1+ i))
                    (setf pending :sym))
                   ((char= character #\,) (tok :punct (if (eql next #\@) (+ i 2) (1+ i))))
                   ((char= character #\#) (lisp-dispatch text i next #'tok
                                                         (lambda (face) (setf pending face))))
                   (t
                    (let* ((end (scan-lisp-token text i))
                           (token (subseq text i end))
                           (face (lisp-token-face token))
                           (definer (and after-open (lisp-definer-face token face))))
                      (cond (pending (tok (if (eq face :num) :num pending) end))
                            (definer (tok :kw end) (setf pending definer))
                            (t (tok face end)))))))))
    i))

(defun lisp-dispatch (text i next tok set-pending)
  "Lex a # reader macro at I. TOK emits a token; SET-PENDING sets the face of
the following token."
  (let ((sub (and next (char-downcase next))))
    (cond
      ((null next) (funcall tok :punct (1+ i)))
      ((char= next #\')
       (funcall tok :punct (+ i 2))
       (funcall set-pending :fn))
      ((char= next #\\)
       ;; #\x, #\( and named characters such as #\Space.
       (let ((end (min (length text) (+ i 3))))
         (when (and (< (+ i 2) (length text)) (alphanumericp (char text (+ i 2))))
           (setf end (scan-while text end (lambda (c) (or (alphanumericp c) (find c "-_"))))))
         (funcall tok :str end)))
      ((find sub "xbo") (funcall tok :num (scan-lisp-token text (+ i 2))))
      ((digit-char-p next)
       (let ((end (scan-while text (1+ i) #'digit-char-p)))
         (if (and (< end (length text)) (char-equal (char text end) #\r))
             (funcall tok :num (scan-lisp-token text (1+ end)))
             (funcall tok :punct (min (length text) (1+ end))))))
      ((char= next #\:) (funcall tok :sym (scan-lisp-token text (+ i 2))))
      ((find next "+-")
       (funcall tok :punct (+ i 2))
       (funcall set-pending :const))
      ((find next "([") (funcall tok :punct (1+ i)))
      (t (funcall tok :punct (+ i 2))))))

;;; C-family languages, driven by a per-language spec (a plist).

(defparameter *rust-spec*
  '(:keywords ("as" "async" "await" "break" "const" "continue" "crate" "dyn" "else" "enum"
               "extern" "fn" "for" "if" "impl" "in" "let" "loop" "match" "mod" "move" "mut"
               "pub" "ref" "return" "self" "Self" "static" "struct" "super" "trait" "type"
               "unsafe" "use" "where" "while" "yield" "macro_rules")
    :types ("i8" "i16" "i32" "i64" "i128" "isize" "u8" "u16" "u32" "u64" "u128" "usize"
            "f32" "f64" "bool" "char" "str")
    :constants ("true" "false" "None" "Some" "Ok" "Err")
    :fn-keywords ("fn") :type-keywords ("struct" "enum" "trait" "type" "union")
    :line-comment ("//") :block-comment ("/*" "*/") :nested-comments t
    :strings (#\") :rust t :capitalized-types t))

(defparameter *javascript-spec*
  '(:keywords ("async" "await" "break" "case" "catch" "class" "const" "continue" "debugger"
               "default" "delete" "do" "else" "export" "extends" "finally" "for" "from"
               "function" "if" "import" "in" "instanceof" "let" "new" "of" "return" "static"
               "super" "switch" "this" "throw" "try" "typeof" "var" "void" "while" "with"
               "yield" "get" "set")
    :constants ("true" "false" "null" "undefined" "NaN" "Infinity")
    :fn-keywords ("function") :type-keywords ("class" "extends")
    :line-comment ("//") :block-comment ("/*" "*/")
    :strings (#\" #\') :template t :annotations t :capitalized-types t :ident-extra "$"))

(defparameter *typescript-spec*
  (append '(:keywords ("async" "await" "break" "case" "catch" "class" "const" "continue"
                       "debugger" "default" "delete" "do" "else" "export" "extends" "finally"
                       "for" "from" "function" "if" "import" "in" "instanceof" "let" "new"
                       "of" "return" "static" "super" "switch" "this" "throw" "try" "typeof"
                       "var" "void" "while" "with" "yield" "get" "set" "interface" "type"
                       "enum" "implements" "namespace" "declare" "abstract" "private"
                       "public" "protected" "readonly" "keyof" "as" "is" "infer" "satisfies"
                       "module")
            :types ("string" "number" "boolean" "any" "unknown" "never" "object" "symbol"
                    "bigint")
            :type-keywords ("class" "extends" "interface" "type" "enum" "implements"))
          *javascript-spec*))

(defparameter *c-spec*
  '(:keywords ("auto" "break" "case" "const" "continue" "default" "do" "else" "enum" "extern"
               "for" "goto" "if" "inline" "register" "restrict" "return" "sizeof" "static"
               "struct" "switch" "typedef" "union" "volatile" "while")
    :types ("void" "char" "short" "int" "long" "float" "double" "signed" "unsigned" "bool"
            "size_t" "ssize_t" "int8_t" "int16_t" "int32_t" "int64_t" "uint8_t" "uint16_t"
            "uint32_t" "uint64_t" "FILE" "wchar_t")
    :constants ("NULL" "true" "false" "EOF")
    :type-keywords ("struct" "enum" "union")
    :line-comment ("//") :block-comment ("/*" "*/")
    :strings (#\" #\') :preprocessor t))

(defparameter *cpp-spec*
  (append '(:keywords ("auto" "break" "case" "const" "continue" "default" "do" "else" "enum"
                       "extern" "for" "goto" "if" "inline" "register" "return" "sizeof"
                       "static" "struct" "switch" "typedef" "union" "volatile" "while"
                       "alignas" "alignof" "catch" "class" "constexpr" "const_cast"
                       "decltype" "delete" "dynamic_cast" "explicit" "export" "friend"
                       "mutable" "namespace" "new" "noexcept" "operator" "private"
                       "protected" "public" "reinterpret_cast" "static_assert"
                       "static_cast" "template" "this" "throw" "try" "typeid" "typename"
                       "using" "virtual" "override" "final" "concept" "requires"
                       "co_await" "co_return" "co_yield")
            :constants ("NULL" "true" "false" "nullptr" "EOF")
            :type-keywords ("struct" "enum" "union" "class"))
          *c-spec*))

(defparameter *go-spec*
  '(:keywords ("break" "case" "chan" "const" "continue" "default" "defer" "else"
               "fallthrough" "for" "func" "go" "goto" "if" "import" "interface" "map"
               "package" "range" "return" "select" "struct" "switch" "type" "var")
    :types ("bool" "byte" "complex64" "complex128" "error" "float32" "float64" "int" "int8"
            "int16" "int32" "int64" "rune" "string" "uint" "uint8" "uint16" "uint32"
            "uint64" "uintptr" "any")
    :constants ("true" "false" "nil" "iota")
    :fn-keywords ("func") :type-keywords ("type")
    :line-comment ("//") :block-comment ("/*" "*/")
    :strings (#\" #\') :raw-quotes (#\`)))

(defparameter *java-spec*
  '(:keywords ("abstract" "assert" "break" "case" "catch" "class" "continue" "default" "do"
               "else" "enum" "extends" "final" "finally" "for" "if" "implements" "import"
               "instanceof" "interface" "native" "new" "package" "private" "protected"
               "public" "return" "static" "strictfp" "super" "switch" "synchronized" "this"
               "throw" "throws" "transient" "try" "volatile" "while" "var" "record" "yield"
               "sealed" "permits")
    :types ("boolean" "byte" "char" "short" "int" "long" "float" "double" "void")
    :constants ("true" "false" "null")
    :type-keywords ("class" "interface" "enum" "record")
    :line-comment ("//") :block-comment ("/*" "*/")
    :strings (#\" #\') :annotations t :capitalized-types t))

(defparameter *python-spec*
  '(:keywords ("and" "as" "assert" "async" "await" "break" "class" "continue" "def" "del"
               "elif" "else" "except" "finally" "for" "from" "global" "if" "import" "in"
               "is" "lambda" "nonlocal" "not" "or" "pass" "raise" "return" "try" "while"
               "with" "yield" "match" "case")
    :types ("int" "float" "str" "bool" "list" "dict" "set" "tuple" "bytes" "object")
    :constants ("True" "False" "None")
    :fn-keywords ("def") :type-keywords ("class")
    :line-comment ("#") :strings (#\" #\') :triple-quotes t :annotations t
    :string-prefixes ("r" "b" "f" "u" "rb" "br" "fr" "rf")))

(defparameter *shell-spec*
  '(:keywords ("if" "then" "else" "elif" "fi" "for" "while" "until" "do" "done" "case"
               "esac" "in" "function" "select" "return" "local" "export" "readonly"
               "declare" "unset" "shift" "break" "continue" "exit" "source" "alias" "trap"
               "eval" "exec" "set")
    :line-comment ("#") :comment-needs-space t
    :strings (#\") :raw-quotes (#\') :dollar-vars t))

(defparameter *json-spec*
  '(:constants ("true" "false" "null")
    :line-comment ("//") :block-comment ("/*" "*/")
    :strings (#\") :string-keys t))

(defun spec-ident-char-p (character spec)
  (or (alphanumericp character) (char= character #\_)
      (find character (getf spec :ident-extra ""))))

(defun line-comment-at-p (text i spec)
  (some (lambda (marker)
          (and (prefix-at-p marker text i)
               (or (not (getf spec :comment-needs-space))
                   (zerop i) (whitespace-p (char text (1- i))))))
        (getf spec :line-comment)))

(defun scan-brackets (text start open close)
  "Index past the bracketed group whose OPEN character is at or after START."
  (let ((depth 0))
    (loop for i from start below (length text)
          do (cond ((char= (char text i) open) (incf depth))
                   ((and (char= (char text i) close) (<= (decf depth) 0)) (return (1+ i))))
          finally (return (length text)))))

(defun scan-dollar (text start)
  "Index past a shell parameter expansion such as $x, ${x}, or $1."
  (let ((next (and (< (1+ start) (length text)) (char text (1+ start)))))
    (cond ((null next) (1+ start))
          ((char= next #\{) (scan-brackets text (1+ start) #\{ #\}))
          ((or (alpha-char-p next) (char= next #\_))
           (scan-while text (1+ start) (lambda (c) (or (alphanumericp c) (char= c #\_)))))
          ((or (digit-char-p next) (find next "?#@*!$-")) (+ start 2))
          (t (1+ start)))))

(defun scan-spec-string (text start spec)
  "Index past the string whose opening quote is at START."
  (let ((quote (char text start)))
    (cond ((and (getf spec :triple-quotes)
                (prefix-at-p (make-string 3 :initial-element quote) text start))
           (scan-quoted text (+ start 3) (make-string 3 :initial-element quote)))
          (t (scan-quoted text (1+ start) (string quote)
                          :escapes (not (member quote (getf spec :raw-quotes))))))))

(defun spec-quote-p (character spec)
  (or (member character (getf spec :strings)) (member character (getf spec :raw-quotes))))

(defun scan-rust-raw-string (text start)
  "START is just after the r of r#\"...\"#. Return the index past it, or NIL."
  (let* ((quote (scan-while text start (lambda (c) (char= c #\#))))
         (hashes (- quote start)))
    (when (and (< quote (length text)) (char= (char text quote) #\"))
      (scan-quoted text (1+ quote)
                   (concatenate 'string "\"" (make-string hashes :initial-element #\#))
                   :escapes nil))))

(defun rust-lifetime-end (text start)
  "If START begins a lifetime such as 'a (not a char literal), its end."
  (let ((end (scan-while text (1+ start) (lambda (c) (or (alphanumericp c) (char= c #\_))))))
    (when (and (> end (1+ start))
               (alpha-char-p (char text (1+ start)))
               (not (and (< end (length text)) (char= (char text end) #\'))))
      end)))

(defun classify-identifier (word text end spec)
  "Face for identifier WORD ending at END, ignoring pending context."
  (let ((next (and (< end (length text)) (char text end))))
    (cond ((member word (getf spec :keywords) :test #'string=) :kw)
          ((member word (getf spec :types) :test #'string=) :type)
          ((member word (getf spec :constants) :test #'string=) :const)
          ((eql next #\() :fn)
          ((and (getf spec :capitalized-types) (upper-case-p (char word 0)))
           (if (and (> (length word) 1) (every (lambda (c) (or (upper-case-p c) (digit-char-p c) (char= c #\_))) word))
               :const
               :type)))))

(defun lex-template (text start spec)
  "Lex a JavaScript template literal at START, including ${} substitutions."
  (let ((n (length text)) (begin start) (i (1+ start)))
    (loop
      (cond ((>= i n) (emit :str text begin n) (return n))
            ((char= (char text i) #\\) (incf i 2))
            ((char= (char text i) #\`) (emit :str text begin (1+ i)) (return (1+ i)))
            ((prefix-at-p "${" text i)
             (emit :str text begin i)
             (emit :punct text i (+ i 2))
             (setf i (lex-with-spec text (+ i 2) spec t))
             (when (< i n) (emit :punct text i (1+ i)) (incf i))
             (setf begin i))
            (t (incf i))))))

(defun lex-with-spec (text start spec &optional in-substitution)
  "Lex TEXT from START with SPEC. With IN-SUBSTITUTION, stop before the } that
closes a template substitution and return its index."
  (let ((n (length text)) (i start) (pending nil) (braces 0))
    (loop while (< i n)
          do (let* ((begin i)
                    (character (char text i))
                    (next (and (< (1+ i) n) (char text (1+ i)))))
               (flet ((tok (face end &optional keep-pending)
                        (emit face text begin end)
                        (setf i (min n end))
                        (unless keep-pending (setf pending nil))))
                 (cond
                   ((whitespace-p character) (tok nil (scan-while text i #'whitespace-p) t))
                   ((line-comment-at-p text i spec) (tok :comment (line-end text i) t))
                   ((and (getf spec :block-comment) (prefix-at-p (first (getf spec :block-comment)) text i))
                    (tok :comment (apply #'scan-block-comment text i
                                         (append (getf spec :block-comment)
                                                 (list :nested (getf spec :nested-comments))))
                         t))
                   ((and (getf spec :preprocessor) (char= character #\#) (line-start-p text i))
                    (tok :kw (scan-while text (scan-while text (1+ i) #'blank-p) #'alphanumericp)))
                   ((and (getf spec :rust) (char= character #\#) (member next '(#\[ #\!)))
                    (tok :sym (scan-brackets text i #\[ #\])))
                   ((and (getf spec :rust) (char= character #\') (rust-lifetime-end text i))
                    (tok :sym (rust-lifetime-end text i)))
                   ((and (getf spec :rust) (char= character #\'))
                    (tok :str (scan-quoted text (1+ i) "'")))
                   ((and (getf spec :template) (char= character #\`))
                    (setf i (lex-template text i spec) pending nil))
                   ((spec-quote-p character spec)
                    (let ((end (scan-spec-string text i spec)))
                      (tok (if (and (getf spec :string-keys) (eql (next-nonblank text end) #\:))
                               :fn
                               :str)
                           end)))
                   ((and (getf spec :dollar-vars) (char= character #\$)) (tok :sym (scan-dollar text i)))
                   ((and (getf spec :annotations) (char= character #\@) next (alpha-char-p next))
                    (tok :sym (scan-while text (1+ i) (lambda (c) (or (spec-ident-char-p c spec)
                                                                      (char= c #\.))))))
                   ((or (digit-char-p character)
                        (and (char= character #\.) next (digit-char-p next)))
                    (tok :num (scan-number text i)))
                   ((spec-ident-char-p character spec)
                    (let* ((end (scan-while text i (lambda (c) (spec-ident-char-p c spec))))
                           (word (subseq text i end))
                           (after (and (< end n) (char text end))))
                      (cond
                        ;; String prefixes: Python r"..", f".."; Rust b"..", r#".."#.
                        ((and (getf spec :rust) (member word '("r" "br") :test #'string=)
                              (member after '(#\# #\"))
                              (scan-rust-raw-string text end))
                         (tok :str (scan-rust-raw-string text end)))
                        ((and after (spec-quote-p after spec)
                              (member (string-downcase word)
                                      (append (getf spec :string-prefixes)
                                              (and (getf spec :rust) '("b")))
                                      :test #'string=))
                         (tok :str (scan-spec-string text end spec)))
                        ((and (getf spec :rust) (eql after #\!)
                              (not (and (< (1+ end) n) (char= (char text (1+ end)) #\=))))
                         (tok :fn (1+ end)))
                        (pending (tok pending end))
                        (t
                         (let ((face (classify-identifier word text end spec)))
                           (tok face end)
                           (cond ((member word (getf spec :fn-keywords) :test #'string=)
                                  (setf pending :fn))
                                 ((member word (getf spec :type-keywords) :test #'string=)
                                  (setf pending :type))))))))
                   ((and in-substitution (char= character #\}) (zerop braces))
                    (return-from lex-with-spec i))
                   (t
                    (case character (#\{ (incf braces)) (#\} (decf braces)))
                    (tok :punct (1+ i)))))))
    i))

;;; Line-oriented lexers.

(defun config-key-end (text start eol yaml)
  "Position of the key separator (= for TOML, : for YAML) on this line, or NIL."
  (loop with i = start
        while (< i eol)
        do (let ((character (char text i)))
             (cond ((find character "\"'")
                    (setf i (min eol (scan-quoted text (1+ i) (string character)))))
                   ((char= character #\#) (return nil))
                   ((and (not yaml) (char= character #\=)) (return i))
                   ((and yaml (char= character #\:)
                         (or (= (1+ i) eol) (whitespace-p (char text (1+ i)))))
                    (return i))
                   ((and (not yaml) (find character "[{,")) (return nil))
                   (t (incf i))))))

(defun lex-config-value (text start eol)
  (let ((i start))
    (loop while (< i eol)
          do (let ((begin i) (character (char text i)))
               (flet ((tok (face end) (emit face text begin end) (setf i (min eol end))))
                 (cond
                   ((blank-p character) (tok nil (scan-while text i #'blank-p)))
                   ((and (char= character #\#) (or (= i start) (blank-p (char text (1- i)))))
                    (tok :comment eol))
                   ((find character "\"'") (tok :str (scan-quoted text (1+ i) (string character))))
                   ((find character "[]{},") (tok :punct (1+ i)))
                   (t
                    (let* ((end (or (position-if (lambda (c) (or (blank-p c) (find c "[]{},")))
                                             text :start i :end eol)
                                eol))
                           (word (subseq text i end)))
                      (tok (cond ((member word '("true" "false" "null" "~" "yes" "no" "on" "off")
                                          :test #'string-equal)
                                  :const)
                                 ((and (or (digit-char-p character)
                                           (and (find character "+-.") (> (length word) 1)
                                                (digit-char-p (char word 1))))
                                       (= (scan-number text (if (find character "+-") (1+ i) i)) end))
                                  :num))
                           end)))))))))

(defun lex-config (text yaml)
  "Lex TOML (or YAML when YAML is true): comments, sections, keys, and values."
  (let ((n (length text)) (i 0))
    (loop while (< i n)
          do (let ((eol (line-end text i)))
               (let ((indent (min eol (scan-while text i #'blank-p))))
                 (emit nil text i indent)
                 (setf i indent))
               (cond
                 ((and (< i eol) (char= (char text i) #\#)) (emit :comment text i eol))
                 ((and (not yaml) (< i eol) (char= (char text i) #\[)) (emit :type text i eol))
                 ((and yaml (or (prefix-at-p "---" text i) (prefix-at-p "..." text i)))
                  (emit :punct text i eol))
                 (t
                  (when (and yaml (prefix-at-p "- " text i))
                    (emit :punct text i (1+ i))
                    (let ((next (min eol (scan-while text (1+ i) #'blank-p))))
                      (emit nil text (1+ i) next)
                      (setf i next)))
                  (let ((key-end (config-key-end text i eol yaml)))
                    (when key-end
                      (let ((name-end (1+ (or (position-if-not #'blank-p text :end key-end
                                                                          :from-end t)
                                              (1- i)))))
                        (emit :fn text i name-end)
                        (emit nil text name-end key-end))
                      (emit :punct text key-end (1+ key-end))
                      (setf i (1+ key-end))))
                  (lex-config-value text i eol)))
               (emit nil text eol (1+ eol))
               (setf i (1+ eol))))))

(defun lex-diff (text)
  (let ((n (length text)) (i 0))
    (loop while (< i n)
          do (let ((eol (line-end text i)))
               (emit (cond ((or (prefix-at-p "+++" text i) (prefix-at-p "---" text i)) :bold)
                           ((prefix-at-p "@@" text i) :hunk)
                           ((prefix-at-p "+" text i) :added)
                           ((prefix-at-p "-" text i) :removed)
                           ((or (prefix-at-p "diff " text i) (prefix-at-p "index " text i)) :dim))
                     text i eol)
               (emit nil text eol (1+ eol))
               (setf i (1+ eol))))))

;;; Languages.

(defparameter *language-aliases*
  '((:lisp "lisp" "common-lisp" "commonlisp" "cl" "elisp" "emacs-lisp" "scheme" "scm"
     "racket" "clojure" "clj" "cljs" "asd")
    (:rust "rust" "rs")
    (:javascript "javascript" "js" "jsx" "mjs" "cjs" "node")
    (:typescript "typescript" "ts" "tsx" "mts")
    (:c "c" "h")
    (:cpp "cpp" "c++" "cc" "cxx" "hpp" "hh" "hxx")
    (:go "go" "golang")
    (:java "java")
    (:python "python" "py" "python3" "py3")
    (:shell "shell" "sh" "bash" "zsh" "ksh")
    (:json "json" "jsonc" "json5")
    (:toml "toml" "ini")
    (:yaml "yaml" "yml")
    (:diff "diff" "patch" "udiff"))
  "Language keyword followed by the fence info strings that name it.")

(defun language-from-info (info)
  "The language keyword named by INFO (a keyword or fence info string), or NIL."
  (cond ((null info) nil)
        ((keywordp info) (and (assoc info *language-aliases*) info))
        (t (let* ((trimmed (string-trim '(#\Space #\Tab #\{ #\.) (string info)))
                  (word (string-downcase
                         (subseq trimmed 0 (position-if (lambda (c) (or (whitespace-p c) (find c ",}")))
                                                        trimmed)))))
             (car (find-if (lambda (entry) (member word (cdr entry) :test #'string=))
                           *language-aliases*))))))

(defun mostly-balanced-lisp-p (code)
  (let ((open (count #\( code)) (close (count #\) code)))
    (and (plusp open) (<= (abs (- open close)) (max 1 (floor open 10))))))

(defun guess-language (code)
  "Heuristic language for an unlabelled block, or NIL."
  (let* ((trimmed (string-left-trim '(#\Space #\Tab #\Newline #\Return) code))
         (first-line (subseq trimmed 0 (line-end trimmed 0))))
    (flet ((line-starting-p (prefix)
             (loop for line in (uiop:split-string code :separator '(#\Newline))
                   thereis (prefix-at-p prefix (string-left-trim '(#\Space #\Tab) line) 0))))
      (cond ((zerop (length trimmed)) nil)
            ((or (prefix-at-p "diff --git" trimmed 0) (prefix-at-p "--- " trimmed 0)
                 (prefix-at-p "@@ " trimmed 0))
             :diff)
            ((prefix-at-p "#!" trimmed 0)
             (cond ((search "python" first-line) :python)
                   ((search "sh" first-line) :shell)))
            ((and (find (char trimmed 0) "(;") (mostly-balanced-lisp-p code)) :lisp)
            ((or (line-starting-p "fn ") (line-starting-p "pub fn ") (search "let mut " code))
             :rust)
            ((and (find (char trimmed 0) "{[") (search "\":" code)) :json)))))

(defun detect-language (code &optional info)
  "Language keyword for CODE: from the fence INFO string when one is given,
otherwise by simple heuristics. NIL means plain text."
  (if (and info (plusp (length (string-trim " " (string info)))))
      (language-from-info info)
      (guess-language code)))

(defun lex-code (code language)
  "Tokens (FACE . TEXT) for CODE in LANGUAGE (a keyword or info string).
Adjacent tokens with the same face are merged. Unknown languages give one
plain token."
  (let ((language (language-from-info language))
        (*tokens* '()))
    (handler-case
        (progn
          (case language
            (:lisp (lex-lisp code))
            (:rust (lex-with-spec code 0 *rust-spec*))
            (:javascript (lex-with-spec code 0 *javascript-spec*))
            (:typescript (lex-with-spec code 0 *typescript-spec*))
            (:c (lex-with-spec code 0 *c-spec*))
            (:cpp (lex-with-spec code 0 *cpp-spec*))
            (:go (lex-with-spec code 0 *go-spec*))
            (:java (lex-with-spec code 0 *java-spec*))
            (:python (lex-with-spec code 0 *python-spec*))
            (:shell (lex-with-spec code 0 *shell-spec*))
            (:json (lex-with-spec code 0 *json-spec*))
            (:toml (lex-config code nil))
            (:yaml (lex-config code t))
            (:diff (lex-diff code))
            (t (emit nil code 0 (length code)))))
      ;; Belt and braces: a lexer bug must never break the transcript.
      (error () (setf *tokens* (list (cons nil code)))))
    (let ((merged '()))
      (dolist (token (nreverse *tokens*) (nreverse merged))
        (if (and merged (eq (car (first merged)) (car token)))
            (setf (cdr (first merged)) (concatenate 'string (cdr (first merged)) (cdr token)))
            (push token merged))))))

(defun style-tokens (tokens)
  "Concatenate TOKENS, styling each line of each token separately so every
output line carries its own complete SGR state."
  (with-output-to-string (out)
    (loop for (face . text) in tokens
          do (if face
                 (loop for (piece . more) on (uiop:split-string text :separator '(#\Newline))
                       do (write-string (styled face piece) out)
                          (when more (terpri out)))
                 (write-string text out)))))

(defun highlight-code (code language)
  "Return CODE highlighted for LANGUAGE (a keyword or fence info string).
Plain CODE comes back when colour is off or the language is unknown."
  (if (and *color* (language-from-info language))
      (style-tokens (lex-code code language))
      code))
