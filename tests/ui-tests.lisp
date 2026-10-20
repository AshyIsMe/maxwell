(in-package #:maxwell/tests)

;;; Styling, syntax highlighting, markdown, and transcript rendering.

(defun esc (string)
  "STRING with each ~ replaced by ESC, for writing expected escape codes."
  (substitute (code-char 27) #\~ string))

(defun faces (code language)
  "Token faces and texts for CODE, without whitespace-only plain tokens."
  (remove-if (lambda (token) (and (null (car token)) (every #'maxwell::whitespace-p (cdr token))))
             (maxwell::lex-code code language)))

(defun test-style ()
  (let ((maxwell::*color* nil))
    (let ((text "hello"))
      (check (eq (maxwell::styled :error text) text)))
    (check (string= (with-output-to-string (out)
                      (maxwell::with-face (:dim out) (write-string "x" out)))
                    "x"))
    (check (equalp (maxwell::ansi-wrap (format nil "abc~Cd~%~%e" #\Tab) 4)
                   #("abc " "    " "d" "" "e"))))
  (let ((maxwell::*color* t))
    (check (string= (maxwell::styled :error "bad") (esc "~[31mbad~[0m")))
    (check (string= (maxwell::styled :error "") ""))
    (check (string= (maxwell::styled :assistant "plain") "plain"))
    (check (string= (with-output-to-string (out)
                      (maxwell::with-face (:dim out) (write-string "x" out)))
                    (esc "~[2mx~[0m")))
    (check (= (maxwell::visible-length (maxwell::styled :heading "Title")) 5))
    (check (string= (maxwell::strip-ansi (format nil "~A~C~C" (esc "a~[1;31mb~[0m~]0;title") (code-char 7) #\c)) "abc"))
    ;; Wrapping closes the SGR state at each break and reopens it after.
    (check (equalp (maxwell::ansi-wrap (concatenate 'string (maxwell::styled :error "abcdef") "gh") 3)
                   (vector (esc "~[31mabc~[0m") (esc "~[31mdef~[0m") "gh")))
    (check (equalp (maxwell::ansi-wrap (format nil "~A~%~A" (esc "~[1m~[33mab") (esc "cd~[0m")) 10)
                   (vector (esc "~[1m~[33mab~[0m") (esc "~[1;33mcd~[0m"))))
    (check (equalp (maxwell::ansi-wrap (esc "~[38;5;0mab~[0m") 1)
                   (vector (esc "~[38;5;0ma~[0m") (esc "~[38;5;0mb~[0m"))))
    ;; Non-SGR escapes and control characters are dropped; unclosed state is closed.
    (check (equalp (maxwell::ansi-wrap (format nil "~Aa~Cb~A" (esc "~[2J") (code-char 1) (esc "~[31mc")) 80)
                   (vector (esc "ab~[31mc~[0m"))))
    (check (every (lambda (line) (<= (maxwell::visible-length line) 7))
                  (maxwell::ansi-wrap (maxwell::highlight-code "(defun foo (x) (* x 2)) ; comment" :lisp) 7))))
  (check (eq (maxwell::detect-color :always) t))
  (check (null (maxwell::detect-color :never)))
  (let ((maxwell::*color* nil))
    (maxwell::configure-color "always")
    (check (eq maxwell::*color* t))
    (maxwell::configure-color :never)
    (check (null maxwell::*color*))
    (check (handler-case (progn (maxwell::configure-color "sometimes") nil)
             (error () t)))))

(defun test-lisp-lexer ()
  (check (equal (faces "(defun foo (x &optional (y 1/2)) \"doc\" ; c" :lisp)
                '((:punct . "(") (:kw . "defun") (:fn . "foo") (:punct . "(")
                  (nil . "x ") (:kw . "&optional") (:punct . "(") (nil . "y ")
                  (:num . "1/2") (:punct . "))") (:str . "\"doc\"") (:comment . "; c"))))
  (check (equal (faces "#| a #| nested |# b |# :key 'sym #'car #\\Space #\\( #x1F #b101 -1.5e3" :lisp)
                '((:comment . "#| a #| nested |# b |#") (:sym . ":key") (:punct . "'")
                  (:sym . "sym") (:punct . "#'") (:fn . "car") (:str . "#\\Space") (:str . "#\\(")
                  (:num . "#x1F") (:num . "#b101") (:num . "-1.5e3"))))
  (check (equal (faces "`(a ,b ,@c) |Piped sym| t nil pi cl:mapcar let" :lisp)
                '((:punct . "`(") (nil . "a ") (:punct . ",") (nil . "b ") (:punct . ",@")
                  (nil . "c") (:punct . ")") (nil . " |Piped sym| ")
                  (:const . "t") (:const . "nil") (:const . "pi") (:fn . "cl:mapcar")
                  (:kw . "let"))))
  (check (equal (faces "(defstruct point x) (defmacro m ()) (define-thing w)" "lisp")
                '((:punct . "(") (:kw . "defstruct") (:type . "point") (nil . " x")
                  (:punct . ")") (:punct . "(") (:kw . "defmacro") (:fn . "m")
                  (:punct . "())") (:punct . "(") (:kw . "define-thing") (:fn . "w")
                  (:punct . ")"))))
  (let ((maxwell::*rainbow-parens* t))
    (check (equal (faces "((a))" :lisp)
                  '((:punct . "(") (:dim . "(") (nil . "a") (:dim . ")") (:punct . ")"))))))

(defun test-c-family-lexers ()
  (check (equal (faces "#[derive(Debug)] fn main<'a>(s: &'a str) { println!(\"{}\", r#\"x\"y\"#); }" :rust)
                '((:sym . "#[derive(Debug)]") (:kw . "fn") (:fn . "main") (:punct . "<")
                  (:sym . "'a") (:punct . ">(") (nil . "s") (:punct . ":") (:punct . "&")
                  (:sym . "'a") (:type . "str") (:punct . ")") (:punct . "{")
                  (:fn . "println!") (:punct . "(") (:str . "\"{}\"") (:punct . ",")
                  (:str . "r#\"x\"y\"#") (:punct . ");") (:punct . "}"))))
  (check (equal (faces "/* a /* b */ c */ let mut v: Vec<u8> = 1_000u32;" "rs")
                '((:comment . "/* a /* b */ c */") (:kw . "let") (:kw . "mut") (nil . " v")
                  (:punct . ":") (:type . "Vec") (:punct . "<") (:type . "u8")
                  (:punct . ">") (:punct . "=") (:num . "1_000u32") (:punct . ";"))))
  (check (equal (faces "const s = `a ${f(x)} b`; // c" "js")
                '((:kw . "const") (nil . " s ") (:punct . "=") (:str . "`a ")
                  (:punct . "${") (:fn . "f") (:punct . "(") (nil . "x")
                  (:punct . ")}") (:str . " b`") (:punct . ";") (:comment . "// c"))))
  (check (equal (faces "interface A { x: number } let y = null" "tsx")
                '((:kw . "interface") (:type . "A") (:punct . "{") (nil . " x") (:punct . ":")
                  (:type . "number") (:punct . "}") (:kw . "let") (nil . " y ") (:punct . "=")
                  (:const . "null"))))
  (check (equal (faces "#include <x.h>
int f(void) { return 0x1F; }" "c")
                '((:kw . "#include") (:punct . "<") (nil . "x") (:punct . ".") (nil . "h")
                  (:punct . ">") (:type . "int") (:fn . "f") (:punct . "(") (:type . "void")
                  (:punct . ")") (:punct . "{") (:kw . "return") (:num . "0x1F")
                  (:punct . ";") (:punct . "}"))))
  (check (equal (faces "class A { nullptr; }" "cpp")
                '((:kw . "class") (:type . "A") (:punct . "{") (:const . "nullptr")
                  (:punct . ";") (:punct . "}"))))
  (check (equal (faces "func Run() error { s := `raw\\`; return nil }" "go")
                '((:kw . "func") (:fn . "Run") (:punct . "()") (:type . "error")
                  (:punct . "{") (nil . " s ") (:punct . ":=") (:str . "`raw\\`")
                  (:punct . ";") (:kw . "return") (:const . "nil") (:punct . "}"))))
  (check (equal (faces "@Override public String name() { return MAX; }" "java")
                '((:sym . "@Override") (:kw . "public") (:type . "String") (:fn . "name")
                  (:punct . "()") (:punct . "{") (:kw . "return") (:const . "MAX")
                  (:punct . ";") (:punct . "}")))))

(defun test-small-lexers ()
  (check (equal (faces "@dec
def f(x):
    '''doc ' more'''  # c
    return rb'x' or None" "python")
                '((:sym . "@dec") (:kw . "def") (:fn . "f") (:punct . "(") (nil . "x")
                  (:punct . "):") (:str . "'''doc ' more'''") (:comment . "# c")
                  (:kw . "return") (:str . "rb'x'") (:kw . "or") (:const . "None"))))
  (check (equal (faces "for f in *.txt; do echo \"$f\" '$x' ${HOME}; done # c" "bash")
                '((:kw . "for") (nil . " f ") (:kw . "in") (:punct . "*.") (nil . "txt")
                  (:punct . ";") (:kw . "do") (nil . " echo ") (:str . "\"$f\"") (:str . "'$x'")
                  (:sym . "${HOME}") (:punct . ";") (:kw . "done") (:comment . "# c"))))
  (check (equal (faces "{\"a\": [1, true, null], \"b\": \"s\"}" "json")
                '((:punct . "{") (:fn . "\"a\"") (:punct . ":") (:punct . "[") (:num . "1")
                  (:punct . ",") (:const . "true") (:punct . ",") (:const . "null")
                  (:punct . "],") (:fn . "\"b\"") (:punct . ":") (:str . "\"s\"")
                  (:punct . "}"))))
  (check (equal (faces "[server]
port = 8080 # c
name = \"x\"" "toml")
                '((:type . "[server]") (:fn . "port") (:punct . "=") (:num . "8080")
                  (:comment . "# c") (:fn . "name") (:punct . "=") (:str . "\"x\""))))
  (check (equal (faces "key: value
- item: yes # c
url: http://x" "yaml")
                '((:fn . "key") (:punct . ":") (nil . " value
") (:punct . "-") (:fn . "item") (:punct . ":") (:const . "yes") (:comment . "# c")
                  (:fn . "url") (:punct . ":") (nil . " http://x"))))
  (check (equal (faces "--- a
+++ b
@@ -1 +1 @@
-old
+new
 same" "diff")
                '((:bold . "--- a") (:bold . "+++ b") (:hunk . "@@ -1 +1 @@")
                  (:removed . "-old") (:added . "+new") (nil . "
 same")))))

(defun test-highlight-robustness ()
  ;; Tokens always reassemble the input, and malformed input never signals.
  (let ((inputs (list "\"unterminated" "#| never closed" "(((" ")))" "#" "#\\" "'" "`${"
                      "`${ {" "r#\"raw" "/* open" "'''abc" "#[" "$" "${" "@" "\\" "|abc" "#x"
                      "1e+" "\"\\" "#+" "0x" "'a" (coerce (loop for code from 0 below 300
                                                                  collect (code-char code))
                                                            'string))))
    (dolist (language '(:lisp :rust :javascript :typescript :c :cpp :go :java :python
                        :shell :json :toml :yaml :diff nil))
      (dolist (input inputs)
        (let ((tokens (handler-case (maxwell::lex-code input language)
                        (error () :signalled))))
          (check (and (listp tokens)
                      (string= input (format nil "~{~A~}" (mapcar #'cdr tokens))))))))))

(defun test-language-detection ()
  (check (eq (maxwell::detect-language "" "common-lisp") :lisp))
  (check (eq (maxwell::detect-language "" "{.rs}") :rust))
  (check (eq (maxwell::detect-language "" "TSX title=x") :typescript))
  (check (eq (maxwell::detect-language "" "zsh") :shell))
  (check (null (maxwell::detect-language "(defun f ())" "text")))
  (check (eq (maxwell::detect-language "(defun f (x)
  (* x 2))") :lisp))
  (check (eq (maxwell::detect-language "use std::io;
fn main() {}") :rust))
  (check (eq (maxwell::detect-language "let mut x = 1;") :rust))
  (check (eq (maxwell::detect-language "@@ -1 +1 @@") :diff))
  (check (null (maxwell::detect-language "just some prose")))
  (let ((maxwell::*color* nil))
    (check (string= (maxwell::highlight-code "(car x)" :lisp) "(car x)")))
  (let ((maxwell::*color* t))
    (check (string= (maxwell::highlight-code "(car x)" "lisp") (esc "(~[34mcar~[0m x)")))
    (check (string= (maxwell::highlight-code "(car x)" "klingon") "(car x)"))
    ;; Multi-line tokens are styled line by line.
    (check (string= (maxwell::highlight-code (format nil "#| a~%b |#") :lisp)
                    (concatenate 'string (esc "~[2m#| a~[0m") (string #\Newline)
                                 (esc "~[2mb |#~[0m"))))))

(defun test-markdown ()
  (let ((text (format nil "# Title~%Some **bold**, *it*, _it_ and `code`.~%- item~%```lisp~%(car \"*x*\")~%```~%snake_case_name | 2 * 3 * 4")))
    (let ((maxwell::*color* nil))
      (check (eq (maxwell::render-markdown text) text)))
    (let* ((maxwell::*color* t)
           (rendered (maxwell::render-markdown text))
           (plain (maxwell::strip-ansi rendered)))
      (check (string= plain (format nil "Title~%Some bold, it, it and code.~%• item~%  (car \"*x*\")~%snake_case_name | 2 * 3 * 4")))
      (check (search (esc "~[1;35mTitle~[0m") rendered))
      (check (search (esc "~[1mbold~[0m") rendered))
      (check (search (esc "~[3mit~[0m") rendered))
      (check (search (esc "~[36mcode~[0m") rendered))
      (check (search (esc "~[2m•~[0m") rendered))
      ;; Fenced code is highlighted, not treated as markdown.
      (check (search (esc "~[32m\"*x*\"~[0m") rendered))))
  (let ((maxwell::*color* t))
    (flet ((plain (text) (maxwell::strip-ansi (maxwell::render-markdown text))))
      (check (string= (plain "a_b_c and x__y__z and file_name_here") "a_b_c and x__y__z and file_name_here"))
      (check (string= (plain "**bold *nested* bold**") "bold nested bold"))
      (check (string= (plain "unclosed **bold and `tick") "unclosed **bold and `tick"))
      (check (string= (plain "  * nested item") "  • nested item"))
      (check (string= (plain "3. third") "3. third"))
      (check (string= (plain "| a | **b** |") "| a | **b** |"))
      (check (string= (plain "> see [docs](http://x)") "> see [docs](http://x)"))
      (check (search (esc "~[2m[docs](http://x)~[0m") (maxwell::render-markdown "[docs](http://x)")))
      (check (string= (plain "####### seven") "####### seven"))
      (check (string= (plain (format nil "~~~~~~~%unterminated *x*")) "  unterminated *x*"))
      ;; A heading's own face resumes after an inline span.
      (check (string= (maxwell::render-markdown "## A `b` c")
                      (esc "~[1;35mA ~[36mb~[0m~[1;35m c~[0m"))))))

(defun entry-vector (&rest entries)
  (let ((vector (maxwell::make-entry-vector)))
    (dolist (entry entries vector) (vector-push-extend entry vector))))

(defun rows-text (rows)
  (format nil "~{~A~^~%~}" (coerce rows 'list)))

(defun test-tool-blocks ()
  (let* ((code (format nil "(defun parse-header (line)~%  (subseq line 0 4))~%(parse-header \"abcdef\")"))
         (result (format nil "~{line ~D~^~%~}~%=> \"abcd\"" (loop for i from 1 to 5 collect i)))
         (entries (entry-vector (maxwell::make-entry :tool code)
                                (maxwell::make-entry :echo result))))
    (let ((maxwell::*color* nil))
      ;; Summary: the first form on one line, the last result lines, a count.
      (check (equal (coerce (maxwell::entry-rows entries 0 60 :summary) 'list)
                    '("• lisp (defun parse-header (line) (subseq line 0 4)) …"
                      "  └ line 4"
                      "    line 5"
                      "    => \"abcd\""
                      "    … +5 lines (ctrl-t to expand)"
                      "")))
      ;; The result is drawn with its call, not again on its own.
      (check (zerop (length (maxwell::entry-rows entries 1 60 :summary))))
      ;; A long first form is cut to the width.
      (let ((rows (maxwell::entry-rows
                    (entry-vector (maxwell::make-entry
                                    :tool (format nil "(list ~{~D ~})" (loop for i below 50 collect i)))
                                  (maxwell::make-entry :echo "ok"))
                    0 30 :summary)))
        (check (= 30 (length (aref rows 0))))
        (check (char= #\… (char (aref rows 0) 29))))
      ;; Full: all code, then the whole result.
      (check (equal (coerce (maxwell::entry-rows entries 0 60 :full) 'list)
                    '("• lisp"
                      "    (defun parse-header (line)"
                      "      (subseq line 0 4))"
                      "    (parse-header \"abcdef\")"
                      "  └ line 1"
                      "    line 2" "    line 3" "    line 4" "    line 5"
                      "    => \"abcd\"" ""))))
    (let ((maxwell::*color* t))
      ;; Green bullet on success, red on error, in both modes.
      (check (search (esc "~[32m•~[0m") (aref (maxwell::entry-rows entries 0 60 :summary) 0)))
      (let ((failed (entry-vector (maxwell::make-entry :tool "(error \"x\")")
                                  (maxwell::make-entry :echo "SIMPLE-ERROR: x" :ok-p nil))))
        (dolist (mode '(:summary :full))
          (check (search (esc "~[31m•~[0m") (aref (maxwell::entry-rows failed 0 60 mode) 0))))
        (check (search (esc "~[31mSIMPLE-ERROR: x~[0m")
                       (rows-text (maxwell::entry-rows failed 0 60 :full)))))
      ;; Full mode highlights the code as Lisp; summaries dim the result.
      (check (search (esc "~[35mdefun~[0m") (rows-text (maxwell::entry-rows entries 0 60 :full))))
      (check (search (esc "~[2mline 5~[0m") (rows-text (maxwell::entry-rows entries 0 60 :summary))))
      ;; Other entries: the user gutter, steering echoes, error status, markdown.
      (let ((others (entry-vector (maxwell::make-entry :user "hi")
                                  (maxwell::make-entry :steer "be brief")
                                  (maxwell::make-entry :status "bad" :ok-p nil)
                                  (maxwell::make-entry :assistant "**ok**"))))
        (check (string= (aref (maxwell::entry-rows others 0 40) 0) (esc "~[36m›~[0m ~[1mhi~[0m")))
        (check (string= (aref (maxwell::entry-rows others 1 40) 0)
                        (esc "~[33msteer>~[0m ~[33mbe brief~[0m")))
        (check (string= (aref (maxwell::entry-rows others 2 40) 0) (esc "~[31mbad~[0m")))
        (check (string= (aref (maxwell::entry-rows others 3 40) 0) (esc "~[1mok~[0m")))))
    ;; Without a terminal width nothing wraps and no separator rows are added.
    (let ((maxwell::*color* nil)
          (long (make-string 200 :initial-element #\a)))
      (check (equalp (maxwell::entry-rows (entry-vector (maxwell::make-entry :assistant long)) 0 nil)
                     (vector long))))
    ;; Live printing waits for the result, then prints the pair once.
    (let ((maxwell::*color* nil)
          (maxwell::*session-entries* (maxwell::make-entry-vector))
          (out (make-string-output-stream)))
      (maxwell::record-entry :tool "(+ 1 2)" :show t :stream out)
      (check (string= "" (get-output-stream-string out)))
      (maxwell::record-entry :echo "3" :show t :stream out)
      (check (string= (get-output-stream-string out) (format nil "• lisp (+ 1 2)~%  └ 3~%"))))))

(defun numbered-entries (count)
  "COUNT one-line replies, each two rows tall at a terminal width."
  (let ((vector (maxwell::make-entry-vector)))
    (dotimes (i count vector)
      (vector-push-extend (maxwell::make-entry :assistant (format nil "reply ~D" i)) vector))))

(defun test-lazy-viewer ()
  (let ((maxwell::*color* nil)
        (maxwell::*older-entries-function* nil)
        (entries (numbered-entries 1000)))
    (maxwell::clear-row-cache)
    ;; A redraw renders only about two pages, however long the session.
    (let ((maxwell::*entry-render-count* 0))
      (multiple-value-bind (rows more) (maxwell::transcript-tail entries 79 24)
        (check (= 48 (length rows)))
        (check more)
        (check (string= "reply 999" (first (last rows 2))))
        (check (<= maxwell::*entry-render-count* 30))))
    ;; The viewer opens on the same rows, rendering little more.
    (let* ((maxwell::*entry-render-count* 0)
           (viewer (maxwell::open-viewer entries 79 22)))
      (check (<= maxwell::*entry-render-count* 40))
      ;; Scrolling up renders older batches; the entry a page above comes to
      ;; the top and stays there while batches are added above it.
      (loop repeat 10
            do (let ((count maxwell::*entry-render-count*)
                     (target (nth-value 1 (maxwell::viewer-row
                                            viewer (- (maxwell::viewer-start viewer) 22)))))
                 (maxwell::viewer-scroll viewer -1)
                 (check (eq target (maxwell::viewer-top-entry viewer)))
                 (check (> maxwell::*entry-render-count* count))))
      (check (<= maxwell::*entry-render-count* 160))
      ;; Ctrl-T re-renders around the top entry and keeps it at the top.
      (let ((top (maxwell::viewer-top-entry viewer))
            (maxwell::*entry-render-count* 0))
        (maxwell::clear-row-cache)
        (let ((maxwell::*tool-display* :full))
          (maxwell::viewer-rerender viewer))
        (check (eq top (maxwell::viewer-top-entry viewer)))
        (check (<= maxwell::*entry-render-count* 60)))
      ;; Paging back to the start reaches the first entry.
      (loop repeat 200 do (maxwell::viewer-scroll viewer -1))
      (check (eq (aref entries 0) (maxwell::viewer-top-entry viewer)))
      (check (not (maxwell::viewer-more-above-p viewer)))))
  ;; Older entries come from *OLDER-ENTRIES-FUNCTION* once the session's run
  ;; out, and a tool call split from its result by a batch is drawn whole.
  (let* ((maxwell::*color* nil)
         (log (entry-vector (maxwell::make-entry :user "old question")
                            (maxwell::make-entry :tool "(old)")))
         (session (entry-vector (maxwell::make-entry :echo "OLD-RESULT")
                                (maxwell::make-entry :assistant "new reply")))
         (calls '())
         (maxwell::*older-entries-function*
           (lambda (oldest count)
             (push (list oldest count) calls)
             (if (eq oldest (aref session 0)) log #()))))
    (maxwell::clear-row-cache)
    (let* ((viewer (maxwell::open-viewer session 79 22))
           (text (format nil "~{~A~^~%~}"
                         (loop for k below (maxwell::viewer-row-count viewer)
                               collect (maxwell::viewer-row viewer k)))))
      (check calls)
      (check (search (format nil "› old question~%~%• lisp (old)~%  └ OLD-RESULT") text))
      (check (not (maxwell::viewer-more-above-p viewer))))))

(defun test-recent-exchanges ()
  (let ((maxwell::*color* t)
        (out (make-string-output-stream))
        (entries (entry-vector (maxwell::make-entry :user "one") (maxwell::make-entry :assistant "a")
                               (maxwell::make-entry :user "two") (maxwell::make-entry :assistant "b")
                               (maxwell::make-entry :user "three") (maxwell::make-entry :assistant "c"))))
    (maxwell::show-recent-exchanges entries :count 2 :stream out)
    (let ((text (get-output-stream-string out)))
      (check (not (search "one" text)))
      (check (search (esc "~[2m› two~[0m") text))
      (check (search (esc "~[2mc~[0m") text)))))

(defun test-status-bar ()
  (let ((maxwell::*color* nil)
        (maxwell::*status-bar-enabled* t)
        (maxwell::*status-bar-state* :idle)
        (maxwell::*status-bar-detail* nil)
        (maxwell::*status-bar-visible* nil)
        (maxwell::*status-bar-drawn* "")
        (client (maxwell:make-client :model "gpt-5" :auth-provider :safeai)))
    ;; Which provider and model answers, and what Maxwell is doing.
    (check (string= "safeai/gpt-5  idle" (maxwell::status-bar-text client)))
    (maxwell::set-activity :tooling "common_lisp")
    (check (string= "safeai/gpt-5  tooling · common_lisp"
                    (maxwell::status-bar-text client)))
    (maxwell::set-activity :idle)
    ;; A base URL names the provider when no login is remembered.
    (check (string= "openai" (maxwell::status-bar-host-label "https://api.openai.com/v1")))
    (check (string= "safeailabs"
                    (maxwell::status-bar-host-label "https://us-region.api.safeailabs.ai/v1")))
    (check (string= "localhost"
                    (maxwell::status-bar-host-label "http://localhost:8080/v1")))
    (check (string= "no model" (maxwell::status-bar-model nil)))
    ;; The row is clipped to the terminal width, and nothing is drawn off a
    ;; terminal.
    (check (= 8 (length (maxwell::status-bar-row client 10))))
    (check (string= "…" (subseq (maxwell::status-bar-row client 10) 7)))
    (let ((out (make-string-output-stream)))
      (maxwell::status-bar-show out 40 client)
      (check (string= "" (get-output-stream-string out)))
      (check (null maxwell::*status-bar-visible*)))
    ;; /statusbar turns it off and on again.
    (let ((*standard-output* (make-string-output-stream)))
      (maxwell::status-bar-command "/statusbar off" client)
      (check (null maxwell::*status-bar-enabled*))
      (maxwell::status-bar-command "/statusbar" client)
      (check maxwell::*status-bar-enabled*)
      (maxwell::status-bar-command "/statusbar on" client)
      (check maxwell::*status-bar-enabled*))))

(defun test-thinking-entry ()
  (let ((maxwell::*color* nil)
        (entries (entry-vector (maxwell::make-entry :thinking (format nil "step one~%step two")))))
    ;; The trace was on screen while the model thought, so it is hidden by
    ;; default; Ctrl-T brings the whole thing back with the tool calls.
    (check (zerop (length (maxwell::entry-rows entries 0 60 :summary))))
    (check (equal (coerce (maxwell::entry-rows entries 0 60 :full) 'list)
                  '("  step one" "  step two" "")))
    ;; A hidden trace prints nothing as it arrives.
    (let ((maxwell::*session-entries* (maxwell::make-entry-vector))
          (out (make-string-output-stream)))
      (maxwell::record-entry :thinking "hidden" :show t :stream out)
      (check (string= "" (get-output-stream-string out)))))
  ;; What the model streams while it thinks is drawn above the status row.
  (let ((maxwell::*color* nil)
        (maxwell::*live-reasoning* (format nil "one~%two~%three")))
    (check (equal (maxwell::live-reasoning-rows 60) '("one" "two" "three")))
    (maxwell::note-live-reasoning " four")
    (check (string= (format nil "one~%two~%three four") maxwell::*live-reasoning*))
    (maxwell::hide-reasoning-trace)
    (check (null maxwell::*live-reasoning*))))

(defun run-ui-tests ()
  (test-status-bar)
  (test-tool-blocks)
  (test-thinking-entry)
  (test-lazy-viewer)
  (test-recent-exchanges)
  (test-style)
  (test-lisp-lexer)
  (test-c-family-lexers)
  (test-small-lexers)
  (test-highlight-robustness)
  (test-language-detection)
  (test-markdown))
