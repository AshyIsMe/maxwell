(in-package #:maxwell/tests)

(defun test-json ()
  (let* ((source "{\"name\":\"Maxwell\\n\",\"ok\":true,\"xs\":[1,null,false]}")
         (decoded (maxwell/json:decode source))
         (round-trip (maxwell/json:decode (maxwell/json:encode decoded))))
    (check (string= (maxwell/json:get-key round-trip "name") (format nil "Maxwell~%")))
    (check (eq t (maxwell/json:get-key round-trip "ok")))
    (check (= 3 (length (maxwell/json:get-key round-trip "xs")))))
  (check (string= "1.5e0" (maxwell/json:encode 1.5d0))))

(defun test-json-key-case ()
  (let* ((encoded (maxwell/json:encode
                    (maxwell/json:json-object "additionalProperties" nil)))
         (decoded (maxwell/json:decode encoded)))
    (check (nth-value 1 (gethash "additionalProperties" decoded)))
    (check (not (nth-value 1 (gethash "additionalproperties" decoded))))))

(defun test-vendored-libraries ()
  ;; The pinned copies under vendor/ are the ones loaded, whatever else the
  ;; machine has installed.
  (dolist (name '("alexandria" "fset" "misc-extensions" "named-readtables"))
    (let ((system (asdf:find-system name)))
      (check (search "/vendor/" (namestring (asdf:system-source-directory system))))))
  (check (string= "2.5.0" (asdf:component-version (asdf:find-system "fset"))))
  ;; Agent-authored Lisp reaches both libraries through their nicknames, and
  ;; FSet does not shadow CL's own sequence functions there.
  (check (string= "2" (maxwell:eval-lisp "(values (f:lookup (f:with (f:empty-map) :b 2) :b))")))
  (check (string= "(1 2)" (maxwell:eval-lisp "(a:ensure-list '(1 2))")))
  (check (string= "3" (maxwell:eval-lisp "(find 3 '(1 2 3))")))
  (check (eq (find-symbol "FIND" "MAXWELL/WORKSPACE") 'cl:find)))

(defun test-eval ()
  (check (string= "3" (maxwell:eval-lisp "(+ 1 2)")))
  (check (search "hello" (maxwell:eval-lisp "(format t \"hello\") 9")))
  (maxwell:eval-lisp "(defparameter *remembered* 41)")
  (check (string= "42" (maxwell:eval-lisp "(1+ *remembered*)")))
  ;; The second value tells errors apart without parsing the text.
  (check (null (nth-value 1 (maxwell:eval-lisp "(+ 1 2)"))))
  (multiple-value-bind (text error-p) (maxwell:eval-lisp "(error \"boom\")")
    (check error-p)
    (check (search "SIMPLE-ERROR: boom" text)))
  ;; Output the model reads is never styled.
  (let ((maxwell::*color* t))
    (check (string= "\"(car x)\"" (maxwell:eval-lisp "(maxwell::highlight-code \"(car x)\" :lisp)")))))

(defun test-agent-loop ()
  (let ((responses
          (list (fake-response :tool-calls (maxwell/json:json-array
                                             (tool-call "(defparameter *loop-value* 6) (* *loop-value* 7)")))
                (fake-response :content "The answer is 42."))))
    (let* ((model (lambda (messages tools)
                    (declare (ignore messages tools))
                    (pop responses)))
           (agent (maxwell:make-agent :model-function model)))
      (check (string= "The answer is 42."
                      (maxwell:run "Calculate it." :agent agent :print-response nil)))
      ;; The view, the message, the tool call, its result, and the reply.
      (check (= 5 (length (maxwell:agent-history agent)))))))

(defun test-tool-result-clipping ()
  ;; A tool result the model would choke on is clipped to the same head and
  ;; tail the log keeps, so no single tool message can exceed the limit.
  (let* ((call (tool-call "(+ 1 1)"))
         (huge (concatenate 'string "HEAD" (long-text 70000) "TAIL"))
         (message (maxwell::tool-result-message call huge))
         (content (maxwell::get-key message "content")))
    (check (<= (length content) 30000))
    (check (string= "HEAD" (subseq content 0 4)))
    (check (string= "TAIL" (subseq content (- (length content) 4))))
    (check (search "clipped" content))
    (check (string= (maxwell::clip-tool-output huge) content))))

(defun entries-text (entries)
  (format nil "~{~A~^~%~}" (map 'list #'maxwell::entry-text entries)))

(defun test-nested-agent-run ()
  (let* ((outer (maxwell:make-agent))
         (maxwell:*agent* outer)
         (maxwell:*chat* (maxwell::make-chat))
         (maxwell::*session-entries* (maxwell::make-entry-vector))
         (responses
           (list (fake-response
                   :tool-calls (maxwell/json:json-array
                                 (tool-call "(maxwell/tests::test-agent-loop)")))
                 (fake-response :content "Tests finished.")
                 (fake-response :content "Still here."))))
    (setf (maxwell::agent-model-function outer)
          (lambda (messages tools)
            (declare (ignore messages tools))
            (pop responses)))
    (check (string= "Tests finished."
                    (maxwell:run "Run tests." :agent outer :print-response nil)))
    (check (eq outer maxwell:*agent*))
    ;; The nested run's entries stay out of the session's transcript.
    (check (not (search "Calculate it." (entries-text maxwell::*session-entries*))))
    (check (search "Run tests." (entries-text maxwell::*session-entries*)))
    (check (equal '(:user :tool :echo :assistant)
                  (map 'list #'maxwell::entry-kind maxwell::*session-entries*)))
    ;; The nested run logged its messages in a chat of its own.
    (check (equal '(:user :tool :echo :maxwell)
                  (map 'list #'maxwell::message-kind (maxwell::chat-messages maxwell:*chat*))))
    (check (string= "Still here."
                    (maxwell:run "Continue." :print-response nil)))
    ;; No conversation carries over: each turn starts from the view.
    (check (= 3 (length (maxwell:agent-history outer)))))
  ;; Restore the caller even when a nested provider fails.
  (let* ((outer (maxwell:make-agent))
         (maxwell:*agent* outer)
         (inner (maxwell:make-agent
                  :model-function (lambda (&rest arguments)
                                    (declare (ignore arguments))
                                    nil))))
    (handler-case (maxwell:run "Fail." :agent inner :print-response nil)
      (maxwell::provider-error () nil))
    (check (eq outer maxwell:*agent*))))

(defun test-responses-adapter ()
  (let* ((reasoning (maxwell/json:json-object "type" "reasoning" "id" "rs_1"))
         (function-call
           (maxwell/json:json-object "type" "function_call"
                                     "id" "fc_1"
                                     "call_id" "call_1"
                                     "name" "common_lisp"
                                     "arguments" "{\"code\":\"(+ 20 22)\"}"))
         (wire-response
           (maxwell/json:json-object
             "output" (maxwell/json:json-array reasoning function-call)))
         (chat-response (maxwell::responses-as-chat-response wire-response))
         (message (maxwell/json:get-key
                    (aref (maxwell/json:get-key chat-response "choices") 0)
                    "message"))
         (call (aref (maxwell/json:get-key message "tool_calls") 0))
         (history
           (list (maxwell/json:json-object "role" "system" "content" "system")
                 message
                 (maxwell/json:json-object "role" "tool"
                                           "tool_call_id" "call_1"
                                           "content" "42")))
         (input (maxwell::response-input history)))
    (check (string= "common_lisp"
                    (maxwell/json:get-key (maxwell/json:get-key call "function")
                                          "name")))
    ;; The reasoning item and function call must be replayed before tool output.
    (check (= 3 (length input)))
    (check (eq reasoning (aref input 0)))
    (check (string= "function_call_output"
                    (maxwell/json:get-key (aref input 2) "type")))))

(defun test-responses-sse ()
  (let* ((body
           (format nil
                   "event: response.created~%data: {\"type\":\"response.created\",\"response\":{\"id\":\"resp_1\"}}~%~%event: response.completed~%data: {\"type\":\"response.completed\",\"response\":{\"id\":\"resp_1\",\"output\":[{\"type\":\"message\",\"content\":[{\"type\":\"output_text\",\"text\":\"hello\"}]}]}}~%~%data: [DONE]~%~%"))
         (response (maxwell::decode-responses-sse body))
         (output (maxwell/json:get-key response "output")))
    (check (string= "resp_1" (maxwell/json:get-key response "id")))
    (check (string= "hello"
                    (maxwell/json:get-key
                      (aref (maxwell/json:get-key (aref output 0) "content") 0)
                      "text"))))
  ;; OpenAI ChatGPT-plan streams use store:false. Their terminal response has
  ;; an empty output array, so Maxwell must retain response.output_item.done.
  (let* ((body
           (format nil
                   "event: response.output_item.done~%data: {\"type\":\"response.output_item.done\",\"output_index\":0,\"item\":{\"type\":\"message\",\"content\":[{\"type\":\"output_text\",\"text\":\"rebuilt\"}]}}~%~%event: response.completed~%data: {\"type\":\"response.completed\",\"response\":{\"output\":[]}}~%~%"))
         (response (maxwell::decode-responses-sse body))
         (chat-response (maxwell::responses-as-chat-response response))
         (message (maxwell/json:get-key
                    (aref (maxwell/json:get-key chat-response "choices") 0)
                    "message")))
    (check (string= "rebuilt" (maxwell/json:get-key message "content"))))
  (check
    (handler-case
        (progn
          (maxwell::decode-responses-sse
            (format nil "event: response.failed~%data: {\"type\":\"response.failed\",\"response\":{\"error\":{\"message\":\"bad request\"}}}~%~%"))
          nil)
      (maxwell::provider-error (condition)
        (search "bad request" (maxwell::provider-error-message condition))))))

(defun test-auth-encoding ()
  (let* ((bytes #(0 1 2 253 254 255))
         (encoded (maxwell::base64-encode bytes :url t)))
    (check (equalp bytes (maxwell::base64url-decode encoded))))
  (check (string= "a%20b%2Fc" (maxwell::url-encode "a b/c")))
  (multiple-value-bind (path parameters)
      (maxwell::query-parameters "/auth/callback?code=a%2Fb&state=x+y")
    (check (string= path "/auth/callback"))
    (check (string= "a/b" (cdr (assoc "code" parameters :test #'string=))))
    (check (string= "x y" (cdr (assoc "state" parameters :test #'string=)))))
  (multiple-value-bind (code client-id)
      (maxwell::oauth-result-from-pasted-url
        "http://127.0.0.1:1455/auth/callback?code=secret&state=expected&client_id=oaiapp_test"
        "http://127.0.0.1:1455/auth/callback" "expected" nil)
    (check (string= code "secret"))
    (check (string= client-id "oaiapp_test")))
  (check
    (handler-case
        (progn
          (maxwell::oauth-result-from-pasted-url
            "http://127.0.0.1:1455/auth/callback?code=secret&state=wrong&client_id=oaiapp_test"
            "http://127.0.0.1:1455/auth/callback" "expected" nil)
          nil)
      (error (condition)
        (search "state" (princ-to-string condition) :test #'char-equal))))
  (let ((challenge
          (maxwell::pkce-challenge
            "dBjftJeZ4CVP-mB92K27uhbUJU1p1r_wW1gFWFOEjXk")))
    (check (string= "E9Melhoa2OwvFrEMTJguCHaoeK1t8URWbuGJSstw-cM"
                    challenge)))
  (let ((uuid (maxwell::uuid-v4)))
    (check (= 36 (length uuid)))
    (check (char= #\4 (char uuid 14)))
    (check (find (char-downcase (char uuid 19)) "89ab"))))

(defun test-openai-client-contract ()
  (let* ((client (maxwell:make-client
                   :base-url "https://untrusted.example/v1"
                   :wire-api :chat-completions
                   :auth-provider :openai
                   :model "test"))
         (fields (maxwell::client-request-fields client)))
    (check (string= "https://api.openai.com/v1"
                    (maxwell:client-base-url client)))
    (check (eq :responses (maxwell:client-wire-api client)))
    (check (eq t (maxwell/json:get-key fields "stream")))
    (check (null (maxwell/json:get-key fields "store")))))

(defun test-provider-switching ()
  ;; /model lists every signed-in provider and switches between them.
  (let* ((session (temporary-test-path "model-session"))
         (auth (temporary-test-path "model-openai"))
         (safeai (temporary-test-path "model-safeai"))
         (maxwell::*session-file* session)
         (maxwell::*auth-file* auth)
         (maxwell::*safeai-auth-file* safeai)
         (maxwell::*model-lister*
           (lambda (client)
             (case (maxwell::client-auth-provider client)
               (:openai '(("gpt-a" . "GPT A") ("gpt-b" . "GPT B")))
               (:safeai '(("deepseek/x" . "deepseek/x")))
               (t '(("only" . "only"))))))
         (maxwell::*model-credential-environment* (constantly nil)))
    (unwind-protect
         (flet ((agent (client) (maxwell:make-agent :client client))
                (catalog (agent)
                  (let ((maxwell::*color* nil))
                    (with-output-to-string (out)
                      (let ((*standard-output* out))
                        (maxwell::print-model-catalog agent)))))
                (select (agent model)
                  (let ((maxwell::*color* nil))
                    (with-output-to-string (out)
                      (let ((*standard-output* out))
                        (maxwell::select-model agent model))))))
           (let ((agent (agent (maxwell:make-client :auth-provider :openai
                                                    :model "gpt-a"))))
             ;; With no saved Safe AI Labs key, only this session's provider shows.
             (let ((text (catalog agent)))
               (check (search "gpt-a" text))
               (check (search "gpt-b" text))
               (check (not (search "safeai" text))))
             ;; Saving credentials makes the other provider appear too.
             (maxwell::write-private-json-file
               (maxwell/json:json-object "access_token" "test-token")
               auth)
             (maxwell::write-private-json-file
               (maxwell/json:json-object "api_key" "sail_test")
               safeai)
             (let ((text (catalog agent)))
               (check (search "openai" text))
               (check (search "safeai" text))
               (check (search "deepseek/x" text)))
             ;; A model only the other provider lists switches to it.
             (check (search "deepseek/x" (select agent "deepseek/x")))
             (check (eq :safeai (maxwell::client-auth-provider
                                  (maxwell::agent-client agent))))
             (check (string= "deepseek/x" (maxwell:client-model
                                              (maxwell::agent-client agent))))
             (check (string= "safeai"
                             (maxwell/json:get-key
                               (maxwell/json:get-key (maxwell::load-session-record)
                                                     "provider")
                               "auth_provider")))
             ;; Naming a provider returns to it, keeping a model it lists.
             (check (search "gpt-a" (select agent "openai")))
             (check (eq :openai (maxwell::client-auth-provider
                                  (maxwell::agent-client agent))))
             (check (string= "gpt-a" (maxwell:client-model
                                         (maxwell::agent-client agent))))
             ;; Naming the current provider keeps the ID string, not a catalog cons.
              (check (search "gpt-a" (select agent "openai")))
              (check (string= "gpt-a" (maxwell:client-model
                                         (maxwell::agent-client agent))))
              ;; A model no signed-in provider lists is an error naming them all.
             (check (signals-message-p (lambda () (select agent "nope/model"))
                                       "nope/model" "openai" "safeai")))
           ;; A key in the environment counts as signed in too.
           (let* ((maxwell::*model-credential-environment*
                    (lambda (name)
                      (and (string= name "SAFEAILABS_API_KEY") "sail_env")))
                  (agent (agent (maxwell:make-client :auth-provider :openai
                                                     :model "gpt-a"))))
             (check (search "deepseek/x" (catalog agent)))))
      (dolist (path (list session auth safeai))
        (when (probe-file path) (delete-file path))))))

(defun test-model-catalog ()
  (let* ((response
           (maxwell/json:json-object
             "models" (maxwell/json:json-array
                        (maxwell/json:json-object "slug" "listed"
                                                  "display_name" "Listed"
                                                  "visibility" "list")
                        (maxwell/json:json-object "slug" "hidden"
                                                  "display_name" "Hidden"
                                                  "visibility" "hide"))))
         (models (maxwell::model-catalog-from-response response)))
    (check (= 1 (length models)))
    (check (equal '("listed" . "Listed") (first models))))
  (let ((models
          (maxwell::model-catalog-from-response
            (maxwell/json:json-object
              "data" (maxwell/json:json-array
                       (maxwell/json:json-object "id" "compatible"))))))
    (check (equal '(("compatible" . "compatible")) models))))

(defun test-terminal ()
  (check (eq :toggle-tools (maxwell::terminal-key
                              (make-string-input-stream (string (code-char 20))))))
  (dolist (pair '(("[5~" . :page-up) ("[6~" . :page-down)
                  ("[A" . :up) ("[B" . :down) ("OA" . :up) ("OB" . :down)
                  ("[D" . :left) ("[C" . :right) ("OH" . :home) ("[3~" . :delete)))
    (check (eq (cdr pair)
               (maxwell::terminal-key
                 (make-string-input-stream (format nil "~C~A" #\Escape (car pair)))))))
  (let* ((agent (maxwell:make-agent))
         (output (make-string-output-stream))
         (maxwell::*session-entries* (maxwell::make-entry-vector)))
    (maxwell::record-entry :user "hello")
    ;; Browse, return with Escape, then submit the original draft unchanged.
    (check (string= "draft"
                    (maxwell::read-terminal-prompt
                      agent
                      (make-string-input-stream
                        (format nil "draft~C[5~~~C[6~~~C[6~~~%"
                                #\Escape #\Escape #\Escape))
                      output)))
    (let ((text (get-output-stream-string output)))
      (check (search (format nil "~C[?1049h" #\Escape) text))
      ;; The viewer draws the session's entries.
      (check (search "› hello" text))))
  (let ((maxwell::*agent* nil)
        (*standard-input* (make-string-input-stream (format nil "plain~%")))
        (*standard-output* (make-string-output-stream)))
    (check (string= "plain" (maxwell::read-interactive-prompt nil)))))

(defun test-prompt-editing ()
  (let* ((agent (maxwell:make-agent))
         (maxwell::*prompt-history* '())
         (maxwell::*prompt-kill* "")
         (output (make-string-output-stream)))
    (flet ((feed (keys)
             (maxwell::read-terminal-prompt
               agent (make-string-input-stream keys) output))
           (keys (&rest codes) (format nil "~{~C~}" codes)))
      ;; Ctrl-U discards everything before the cursor.
      (check (string= "abc"
                      (feed (keys #\x #\y #\z (code-char 21) #\a #\b #\c #\Newline))))
      ;; Ctrl-W discards the word before the cursor. Like readline's
      ;; unix-word-rubout, the whitespace preceding that word survives.
      (check (string= "one two "
                      (feed (keys #\o #\n #\e #\Space #\t #\w #\o #\Space
                                  #\t #\h #\r #\e #\e (code-char 23) #\Newline))))
      ;; Ctrl-K discards from the cursor to the end of the line.
      (check (string= "ab"
                      (feed (keys #\a #\b #\c #\d #\Escape #\[ #\D #\Escape #\[ #\D
                                  (code-char 11) #\Newline))))
      ;; Ctrl-Y yanks the text removed by the last kill command.
      (check (string= "!hello"
                      (feed (keys #\h #\e #\l #\l #\o (code-char 21) #\! (code-char 25)
                                  #\Newline))))
      ;; Ctrl-T toggles tool display and redraws instead of editing the line.
      (let ((maxwell::*tool-display* :summary)
            (maxwell::*session-entries* (maxwell::make-entry-vector)))
        (check (string= "ab" (feed (keys #\a #\b (code-char 20) #\Newline))))
        (check (eq maxwell::*tool-display* :full))
        (check (search (format nil "~C[3J" #\Escape) (get-output-stream-string output))))
      ;; Ctrl-A and Ctrl-E jump to the ends of the line.
      (check (string= "abc" (feed (keys #\b #\c (code-char 1) #\a #\Newline))))
      (check (string= "abc" (feed (keys #\a #\b (code-char 1) (code-char 5) #\c #\Newline))))
      ;; Ctrl-D deletes forward; backspace and the Delete key delete backward.
      (check (string= "b" (feed (keys #\a #\b (code-char 1) (code-char 4) #\Newline))))
      (check (string= "ab" (feed (keys #\a #\b #\c (code-char 127) #\Newline))))
      (check (string= "ab"
                      (feed (keys #\a #\b #\c #\Escape #\[ #\D #\Escape #\[ #\3 #\~
                                  #\Newline)))))))

(defun test-prompt-history ()
  (let* ((agent (maxwell:make-agent))
         (maxwell::*prompt-history* '())
         (output (make-string-output-stream))
         (up (format nil "~C[A" #\Escape))
         (down (format nil "~C[B" #\Escape))
         (newline (string #\Newline)))
    (flet ((feed (keys)
             (maxwell::read-terminal-prompt
               agent (make-string-input-stream keys) output))
           (keys (&rest codes) (format nil "~{~C~}" codes)))
      (check (string= "first" (feed (concatenate 'string "first" newline))))
      (check (string= "second" (feed (concatenate 'string "second" newline))))
      (check (equal '("second" "first") maxwell::*prompt-history*))
      ;; Up recalls the most recent prompt, Up again walks further back.
      (check (string= "second" (feed (concatenate 'string up newline))))
      (check (string= "first" (feed (concatenate 'string up up newline))))
      ;; Up past the oldest prompt stays on the oldest prompt.
      (check (string= "first" (feed (concatenate 'string up up up newline))))
      ;; Down returns to the draft line that was being typed.
      (check (string= "draft" (feed (concatenate 'string "draft" up down newline))))
      ;; Ctrl-P and Ctrl-N are Up and Down.
      (check (string= "draft" (feed (keys (code-char 16) #\Newline))))
      ;; Editing a recalled prompt leaves the stored history alone.
      (check (string= "first!" (feed (concatenate 'string up up "!" newline))))
      ;; Blank lines and repeats of the newest entry are not remembered.
      (check (string= "   " (feed (concatenate 'string "   " newline))))
      (check (equal '("first!" "draft" "first" "second" "first")
                    maxwell::*prompt-history*))
      ;; Ctrl-D on an empty line ends the session.
      (check (null (feed (keys (code-char 4))))))))

(defun test-session-memory ()
  (let* ((path (merge-pathnames "maxwell-test-session.json"
                                (uiop:temporary-directory)))
         (maxwell::*session-file* path))
    (unwind-protect
         (progn
           (when (probe-file path) (delete-file path))
           (check (null (maxwell::load-session-record)))
           (maxwell::remember-client
             (maxwell:make-client :base-url "https://remembered.example/v1"
                                  :endpoint "https://remembered.example/v1/chat"
                                  :api-key-environment "REMEMBERED_API_KEY"
                                  :wire-api :chat-completions
                                  :model "remembered-model"
                                  :extra-headers '("X-Remembered: 1")))
           (check (probe-file path))
           (check (string= "600"
                           (string-trim '(#\Newline)
                                        (uiop:run-program
                                          (list "stat" "-c" "%a" (namestring path))
                                          :output :string :error-output nil))))
           (let ((options (maxwell::remembered-client-options
                            (maxwell::load-session-record))))
             (check (equal "https://remembered.example/v1" (getf options :base-url)))
             (check (equal "https://remembered.example/v1/chat"
                           (getf options :endpoint)))
             (check (equal "REMEMBERED_API_KEY" (getf options :api-key-environment)))
             (check (eq :chat-completions (getf options :wire-api)))
             (check (equal "remembered-model" (getf options :model)))
             (check (equal '("X-Remembered: 1") (getf options :extra-headers)))
             (check (null (getf options :auth-provider))))
           ;; The remembered provider and model become the defaults.
           (let ((options (maxwell::resolve-client-options
                            :environment (constantly nil))))
             (check (equal "https://remembered.example/v1" (getf options :base-url)))
             (check (equal "remembered-model" (getf options :model)))
             (check (equal "REMEMBERED_API_KEY" (getf options :api-key-environment)))
             (check (eq :chat-completions (getf options :wire-api))))
           ;; A model alone keeps the remembered provider.
           (let ((options (maxwell::resolve-client-options
                            :model "other-model" :environment (constantly nil))))
             (check (equal "other-model" (getf options :model)))
             (check (equal "https://remembered.example/v1" (getf options :base-url))))
           ;; Naming a provider starts a fresh configuration instead.
           (let ((options (maxwell::resolve-client-options
                            :base-url "https://cli.example/v1" :model "cli-model"
                            :environment (constantly nil))))
             (check (equal "https://cli.example/v1" (getf options :base-url)))
             (check (equal "cli-model" (getf options :model)))
             (check (null (getf options :endpoint)))
             (check (equal "OPENAI_API_KEY" (getf options :api-key-environment))))
           ;; A standing environment preference outranks the remembered session.
           (let ((options
                   (maxwell::resolve-client-options
                     :environment (lambda (name)
                                    (if (string= name "MAXWELL_BASE_URL")
                                        "https://env.example/v1"
                                        nil)))))
             (check (equal "https://env.example/v1" (getf options :base-url)))
             (check (equal "" (getf options :model))))
           ;; A remembered OAuth login keeps its Responses contract.
           (maxwell::remember-client
             (maxwell:make-client :auth-provider :openai :model "codex-model"))
           (let ((client (apply #'maxwell:make-client
                                (maxwell::resolve-client-options
                                  :environment (constantly nil)))))
             (check (string= "https://api.openai.com/v1"
                             (maxwell:client-base-url client)))
             (check (eq :responses (maxwell:client-wire-api client)))
             (check (eq :openai (maxwell::client-auth-provider client)))
             (check (string= "codex-model" (maxwell:client-model client))))
           ;; A damaged session file means "nothing remembered".
           (with-open-file (stream path :direction :output :if-exists :supersede)
             (write-string "{\"model\":" stream))
           (check (null (maxwell::load-session-record)))
           (let ((options (maxwell::resolve-client-options
                            :environment (constantly nil))))
             (check (equal "https://api.openai.com/v1" (getf options :base-url)))
             (check (equal "" (getf options :model)))))
      (when (probe-file path) (delete-file path)))))

(defun test-steering-input ()
  (let ((maxwell::*steering* '())
        (maxwell::*prompt-history* '())
        (maxwell::*prompt-kill* ""))
    (flet ((service (keys &optional (agent (maxwell:make-agent)))
             (let ((output (make-string-output-stream)))
               (maxwell::service-steering-input
                 (maxwell::make-steering-session
                   agent (make-string-input-stream keys) output))
               (get-output-stream-string output))))
      ;; The steering prompt appears as soon as the model starts working.
      (check (search "steer> " (service "")))
      ;; A submitted line is echoed and queued for the model.
      (check (search (format nil "steer> be brief~%") (service (format nil "be brief~%"))))
      (check (equal '("be brief") maxwell::*steering*))
      ;; Editing keys work on the steering line too.
      (check (search (format nil "steer> brief~%") (service (format nil "briefx~C~%" #\Backspace))))
      ;; Blank lines are neither echoed nor queued.
      (let ((output (service (format nil "   ~%"))))
        (check (null (search "steer>    " output))))
      (check (equal '("be brief" "brief") maxwell::*steering*))
      ;; PgUp and PgDn open the transcript viewer while the model works.
      (check (search (format nil "~C[?1049h" #\Escape) (service (format nil "~C[5~~" #\Escape))))
      (check (search (format nil "~C[?1049h" #\Escape) (service (format nil "~C[6~~" #\Escape))))
      ;; The draft survives: text typed before PgUp is still there afterwards.
      (check (search "steer> draft" (service (format nil "draft~C[5~~" #\Escape))))
      ;; Hiding erases the line so model output starts cleanly.
      (let* ((output (make-string-output-stream))
             (session (maxwell::make-steering-session
                        (maxwell:make-agent) (make-string-input-stream "") output)))
        (maxwell::service-steering-input session)
        (maxwell::steering-hide session)
        (check (search (format nil "~C~C[2K" #\Return #\Escape)
                       (get-output-stream-string output)))))))

(defun test-steering-run ()
  ;; A prompt typed while the model works reaches it before the next request.
  (let* ((maxwell:*chat* (maxwell::make-chat))
         (requests '())
         (calls 0)
         (model (lambda (messages tools)
                  (declare (ignore tools))
                  (incf calls)
                  (push (loop for message in (rest messages)
                              collect (maxwell/json:get-key message "content"))
                        requests)
                  (if (= calls 1)
                      (progn (maxwell::queue-steering "be brief")
                             (fake-response
                               :tool-calls (maxwell/json:json-array
                                             (tool-call "(+ 1 1)"))))
                      (fake-response :content "Steered."))))
         (agent (maxwell:make-agent :model-function model)))
    (check (string= "Steered."
                    (maxwell:run "Explain." :agent agent :print-response nil)))
    (check (= 2 calls))
    (check (member "be brief" (first requests) :test #'string=))
    (check (= 6 (length (maxwell:agent-history agent))))
    ;; Steering is logged as the user's words, after the tool call.
    (check (equal '(:user :tool :echo :user :maxwell)
                  (map 'list #'maxwell::message-kind (maxwell::chat-messages maxwell:*chat*))))
    (check (eql 3 (maxwell::entry-id (find :steer maxwell::*session-entries*
                                           :key #'maxwell::entry-kind :from-end t)))))
  ;; A prompt typed while the model is answering continues the turn.
  (let* ((maxwell:*chat* (maxwell::make-chat))
         (calls 0)
         (model (lambda (messages tools)
                  (declare (ignore messages tools))
                  (incf calls)
                  (when (= calls 1) (maxwell::queue-steering "one more thing"))
                  (fake-response :content (format nil "reply ~D" calls))))
         (agent (maxwell:make-agent :model-function model)))
    (check (string= "reply 2" (maxwell:run "Go." :agent agent :print-response nil)))
    (check (= 2 calls))
    (check (= 5 (length (maxwell:agent-history agent))))
    (check (string= "one more thing"
                    (maxwell/json:get-key (fourth (maxwell:agent-history agent))
                                          "content")))))

(defun test-async-command ()
  ;; Commands run synchronously unless the prompt is servicing input.
  (multiple-value-bind (output error-output status)
      (maxwell::run-command '("cat") :input "request body")
    (check (string= "request body" output))
    (check (string= "" error-output))
    (check (zerop status)))
  ;; With an input service installed the command runs asynchronously and the
  ;; service is called while it runs.
  (let ((maxwell::*input-poll* (let ((count 0))
                                 (lambda () (incf count))))
        (maxwell::*input-hide* nil))
    (multiple-value-bind (output error-output status)
        (maxwell::run-command '("cat") :input "steered body")
      (check (string= "steered body" output))
      (check (string= "" error-output))
      (check (zerop status)))
    (let ((polls maxwell::*input-poll*)
          (count 0))
      (declare (ignore polls))
      (let ((maxwell::*input-poll* (lambda () (incf count))))
        (multiple-value-bind (output error-output status)
            (maxwell::run-command '("sleep" "0.15"))
          (declare (ignore output error-output))
          (check (zerop status))
          (check (plusp count)))))
    ;; The hide hook runs before the caller sees the result.
    (let ((hidden nil))
      (let ((maxwell::*input-hide* (lambda () (setf hidden t))))
        (maxwell::run-command '("true")))
      (check hidden)))
  ;; A failing command reports its status instead of signalling.
  (multiple-value-bind (output error-output status)
      (maxwell::run-command '("false"))
    (declare (ignore output error-output))
    (check (= 1 status))))

(defun temporary-test-path (name)
  (merge-pathnames (format nil "maxwell-test-~A-~A.json" name (maxwell::random-value 6))
                   (uiop:temporary-directory)))

(defun signals-message-p (thunk &rest fragments)
  "True when THUNK signals an error whose text contains every one of FRAGMENTS."
  (handler-case (progn (funcall thunk) nil)
    (error (condition)
      (let ((text (princ-to-string condition)))
        (every (lambda (fragment) (search fragment text)) fragments)))))

(defun test-safeai-credentials ()
  (let ((maxwell::*safeai-auth-file* nil))
    ;; MAXWELL_SAFEAI_AUTH_FILE overrides the default location.
    (check (equal (pathname "/elsewhere/key.json")
                  (maxwell::safeai-auth-file-path
                    (lambda (name)
                      (and (string= name "MAXWELL_SAFEAI_AUTH_FILE")
                           "/elsewhere/key.json")))))
    (let ((default (namestring (maxwell::safeai-auth-file-path (constantly nil)))))
      (check (search "maxwell/safeai-auth.json" default))))
  (let* ((path (temporary-test-path "safeai-auth"))
         (maxwell::*safeai-auth-file* path))
    (unwind-protect
         (progn
           (check (signals-message-p
                    (lambda () (maxwell::safeai-api-key :environment (constantly nil)))
                    "/login safeai"))
           (maxwell::write-private-json-file
             (maxwell/json:json-object "api_key" "sail_saved" "model" "saved/model")
             path)
           (check (string= "sail_saved"
                           (maxwell::safeai-api-key :environment (constantly nil))))
           ;; SAFEAILABS_API_KEY takes precedence over the saved key.
           (check (string= "sail_env"
                           (maxwell::safeai-api-key
                             :environment (lambda (name)
                                            (and (string= name "SAFEAILABS_API_KEY")
                                                 "sail_env")))))
           (unless (maxwell::environment-value "SAFEAILABS_API_KEY")
             (check (string= "sail_saved"
                             (maxwell::client-authorization-token
                               (maxwell:make-client :auth-provider :safeai)))))
           (check (maxwell::forget-safeai-login))
           (check (not (probe-file path)))
           (check (not (maxwell::forget-safeai-login))))
      (when (probe-file path) (delete-file path)))))

(defun test-safeai-key-checks ()
  (check (maxwell::safeai-key-problem ""))
  (check (maxwell::safeai-key-problem "sail_ab cd"))
  (check (maxwell::safeai-key-problem (format nil "sail_ab~Ccd" #\Newline)))
  (check (null (maxwell::safeai-key-problem "sail_abcd")))
  (check (null (maxwell::safeai-key-check-message "200" "{\"data\":[]}")))
  (let ((message (maxwell::safeai-key-check-message
                   "401" "{\"error\":{\"code\":\"invalid_api_key\",\"message\":\"no\"}}")))
    (check (search "rejected" message))
    (check (search "invalid_api_key" message))
    (check (search "https://safeailabs.ai/keys" message)))
  (let ((message (maxwell::safeai-key-check-message
                   "403" "{\"error\":{\"code\":\"sail_key_requires_sail\"}}")))
    (check (search "Sail" message))
    (check (search "https://safeailabs.ai/keys" message)))
  ;; The Sail-only code is recognised even in a body that is not JSON.
  (check (search "Sail" (maxwell::safeai-key-check-message
                          "403" "error: sail_key_requires_sail")))
  (check (search "refused" (maxwell::safeai-key-check-message "403" "")))
  (check (search "rate limiting" (maxwell::safeai-key-check-message "429" "")))
  (check (search "HTTP 500" (maxwell::safeai-key-check-message "500" "oops")))
  (check (search "Could not reach"
                 (maxwell::safeai-key-check-message "000" "" 6))))

(defun test-unavailable-model-messages ()
  (let ((message (maxwell::response-error-message
                   "{\"error\":{\"message\":\"slow down\"}}" "429" "")))
    (check (search "rate limiting" message))
    (check (search "slow down" message))
    (check (search "/model" message)))
  (check (search "status 500" (maxwell::response-error-message "oops" "500" "")))
  (let ((client (maxwell:make-client :base-url "https://models.example/v1"
                                     :model "old-model")))
    (let ((note (maxwell::unlisted-model-note
                  client '(("new-model" . "New") ("other" . "other")))))
      (check (search "old-model" note))
      (check (search "new-model, other" note)))
    (check (null (maxwell::unlisted-model-note client '(("old-model" . "Old")))))
    ;; An empty or unreachable catalog proves nothing about the model.
    (check (null (maxwell::unlisted-model-note client '())))
    ;; A failure whose status cannot mean a bad model keeps its message as is.
    (let ((condition (make-condition 'maxwell::provider-error :message "boom")))
      (check (eq condition (maxwell::note-unlisted-model
                             client (format nil "oops~%500") condition))))))

(defun test-private-credential-header ()
  ;; The bearer token travels in an owner-only file, never on the command line.
  (multiple-value-bind (arguments files) (maxwell::bearer-header-arguments "secret-token")
    (check (= 1 (length files)))
    (check (notany (lambda (argument) (search "secret-token" argument)) arguments))
    (check (string= (format nil "@~A" (namestring (car (first files))))
                    (second arguments)))
    (multiple-value-bind (output error-output status)
        (maxwell::run-command (list "cat" (namestring (car (first files))))
                              :private-files files)
      (declare (ignore error-output))
      (check (zerop status))
      (check (string= (format nil "Authorization: Bearer secret-token~%") output)))
    (check (not (probe-file (car (first files))))))
  (multiple-value-bind (arguments files) (maxwell::bearer-header-arguments nil)
    (check (null arguments))
    (check (null files)))
  (check (signals-message-p
           (lambda () (maxwell::bearer-header-arguments (format nil "a~%b")))
           "line break"))
  ;; Asynchronous jobs delete their private files when collected.
  (let* ((path (maxwell::private-temporary-path "test"))
         (job (maxwell::start-command (list "cat" (namestring path))
                                      :private-files (list (cons path "async")))))
    (check (string= "async" (maxwell::finish-command job)))
    (check (not (probe-file path)))))

(defun test-provider-presets ()
  (let* ((session (temporary-test-path "preset-session"))
         (auth (temporary-test-path "preset-safeai"))
         (maxwell::*session-file* session)
         (maxwell::*safeai-auth-file* auth))
    (unwind-protect
         (flet ((resolve (&rest arguments)
                  (apply #'maxwell::resolve-client-options
                         (append arguments (list :environment (constantly nil))))))
           ;; --auth openai alone is the complete ChatGPT-plan preset.
           (let* ((options (resolve :auth-provider "openai"))
                  (client (apply #'maxwell:make-client options))
                  (fields (maxwell::client-request-fields client)))
             (check (eq :openai (getf options :auth-provider)))
             (check (string= "gpt-5.6-sol" (getf options :model)))
             (check (string= "https://api.openai.com/v1" (maxwell:client-base-url client)))
             (check (eq :responses (maxwell:client-wire-api client)))
             (check (eq t (maxwell/json:get-key fields "stream")))
             (check (null (maxwell/json:get-key fields "store"))))
           (check (string= "explicit"
                           (getf (resolve :auth-provider "openai" :model "explicit")
                                 :model)))
           (check (string= "codex-env"
                           (getf (maxwell::resolve-client-options
                                   :auth-provider "OpenAI"
                                   :environment
                                   (lambda (name)
                                     (cond ((string= name "MAXWELL_CODEX_MODEL") "codex-env")
                                           ((string= name "MAXWELL_MODEL") "generic"))))
                                 :model)))
           ;; --auth safeai before any login uses the regional endpoint.
           (let ((options (resolve :auth-provider "safeai")))
             (check (eq :safeai (getf options :auth-provider)))
             (check (string= "https://us-region.api.safeailabs.ai/v1"
                             (getf options :base-url)))
             (check (eq :chat-completions (getf options :wire-api)))
             (check (string= "SAFEAILABS_API_KEY" (getf options :api-key-environment)))
             (check (string= "" (getf options :model))))
           ;; After a login, the saved endpoint and model become the defaults.
           (maxwell::write-private-json-file
             (maxwell/json:json-object "api_key" "sail_k"
                                       "base_url" "https://gbl.api.safeailabs.ai/v1"
                                       "model" "deepseek/deepseek-v4.1-flash")
             auth)
           (let ((options (resolve :auth-provider "safeai")))
             (check (string= "https://gbl.api.safeailabs.ai/v1" (getf options :base-url)))
             (check (string= "deepseek/deepseek-v4.1-flash" (getf options :model))))
           (let ((options (maxwell::resolve-client-options
                            :auth-provider "safeai"
                            :environment
                            (lambda (name)
                              (cond ((string= name "MAXWELL_SAFEAI_MODEL") "env/model")
                                    ((string= name "MAXWELL_SAFEAI_BASE_URL")
                                     "https://safeai.example/v1")
                                    ((string= name "MAXWELL_MODEL") "generic"))))))
             (check (string= "env/model" (getf options :model)))
             (check (string= "https://safeai.example/v1" (getf options :base-url))))
           (check (signals-message-p (lambda () (resolve :auth-provider "nope"))
                                     "openai" "safeai"))
           ;; A Safe AI Labs session is remembered and continued by a bare start,
           ;; without its key ever reaching session.json.
           (maxwell::remember-client
             (apply #'maxwell:make-client (resolve :auth-provider "safeai")))
           (let ((provider (maxwell/json:get-key (maxwell::load-session-record)
                                                 "provider")))
             (check (string= "safeai" (maxwell/json:get-key provider "auth_provider"))))
           (check (not (search "sail_k" (uiop:read-file-string session))))
           (let ((client (apply #'maxwell:make-client (resolve))))
             (check (eq :safeai (maxwell::client-auth-provider client)))
             (check (string= "https://gbl.api.safeailabs.ai/v1"
                             (maxwell:client-base-url client)))
             (check (eq :chat-completions (maxwell:client-wire-api client)))
             (check (string= "deepseek/deepseek-v4.1-flash"
                             (maxwell:client-model client))))
           ;; Adopting Safe AI Labs drops the ChatGPT plan's request contract.
           (let ((client (maxwell::adopt-safeai-client
                           (maxwell:make-client :auth-provider :openai :model "x")
                           "picked/model")))
             (check (eq :safeai (maxwell::client-auth-provider client)))
             (check (eq :chat-completions (maxwell:client-wire-api client)))
             (check (null (maxwell::client-request-fields client)))
             (check (string= "picked/model"
                             (maxwell/json:get-key (maxwell::load-session-record)
                                                   "model")))))
      (dolist (path (list session auth))
        (when (probe-file path) (delete-file path))))))

(defun test-reasoning-streams ()
  (flet ((sse (&rest events)
           (with-output-to-string (out)
             (dolist (event events) (format out "data: ~A~%~%" event)))))
    ;; A streamed Chat Completions reply is rebuilt from its deltas, with the
    ;; reasoning the provider sent.
    (let* ((body (sse "{\"choices\":[{\"delta\":{\"reasoning_content\":\"Thin\"}}]}"
                      "{\"choices\":[{\"delta\":{\"reasoning_content\":\"king\"}}]}"
                      "{\"choices\":[{\"delta\":{\"content\":\"Hi \"}}]}"
                      "{\"choices\":[{\"delta\":{\"content\":\"there\"},\"finish_reason\":\"stop\"}]}"
                      "[DONE]"))
           (response (maxwell::decode-chat-sse body))
           (message (maxwell/json:get-key
                      (aref (maxwell/json:get-key response "choices") 0) "message")))
      (check (string= "Hi there" (maxwell/json:get-key message "content")))
      (check (string= "Thinking" (maxwell::message-reasoning message))))
    ;; Tool calls arrive in pieces and are joined by index.
    (let* ((body (sse "{\"choices\":[{\"delta\":{\"tool_calls\":[{\"index\":0,\"id\":\"call_1\",\"type\":\"function\",\"function\":{\"name\":\"common_lisp\",\"arguments\":\"{\\\"code\\\":\"}}]}}]}"
                      "{\"choices\":[{\"delta\":{\"tool_calls\":[{\"index\":0,\"function\":{\"arguments\":\"\\\"(+ 1 2)\\\"}\"}}]}}]}"))
           (response (maxwell::decode-chat-sse body))
           (message (maxwell/json:get-key
                      (aref (maxwell/json:get-key response "choices") 0) "message"))
           (function (maxwell/json:get-key
                       (aref (maxwell/json:get-key message "tool_calls") 0) "function")))
      (check (string= "common_lisp" (maxwell/json:get-key function "name")))
      (check (string= "{\"code\":\"(+ 1 2)\"}"
                      (maxwell/json:get-key function "arguments"))))
    ;; The live trace hears reasoning as it arrives, even split mid-event.
    (let* ((seen '())
           (maxwell::*reasoning-delta-function* (lambda (text) (push text seen)))
          (reader (maxwell::%make-sse-reader))
          (handler (maxwell::sse-reasoning-handler :chat-completions)))
      (dolist (chunk '("data: {\"choices\":[{\"delta\":{\"reason"
                       "ing_content\":\"A\"}}]}"))
        (maxwell::sse-reader-feed reader chunk handler))
      (maxwell::sse-reader-feed
        reader
        (format nil "~%~%data: {\"choices\":[{\"delta\":{\"reasoning_content\":\"B\"}}]}~%~%")
        handler)
      (check (equal '("A" "B") (nreverse seen))))
    ;; Responses reasoning summaries ride on the message and reach the trace.
    (let* ((reasoning (maxwell/json:json-object
                        "type" "reasoning"
                        "summary" (maxwell/json:json-array
                                     (maxwell/json:json-object
                                       "type" "summary_text" "text" "Let me think."))))
           (response (maxwell::responses-as-chat-response
                       (maxwell/json:json-object
                         "output" (maxwell/json:json-array reasoning))))
           (message (maxwell/json:get-key
                      (aref (maxwell/json:get-key response "choices") 0) "message")))
      (check (string= "Let me think." (maxwell::message-reasoning message))))
    (let* ((seen '())
           (maxwell::*reasoning-delta-function* (lambda (text) (push text seen))))
      (maxwell::responses-sse-reasoning-handler
        "response.reasoning_summary_text.delta"
        "{\"type\":\"response.reasoning_summary_text.delta\",\"delta\":\"hmm\"}")
      (check (equal '("hmm") seen)))
    ;; Reasoning a provider put on a message is never sent back.
    (let ((message (maxwell/json:json-object "role" "assistant" "content" "x"
                                             "reasoning_content" "secret")))
      (maxwell::normalize-message-reasoning message)
      (check (string= "secret" (maxwell::message-reasoning message)))
      (check (not (nth-value 1 (gethash "reasoning_content"
                                        (maxwell::chat-message message))))))
    ;; Chat Completions asks for a stream, and an SSE body is told from JSON.
    (check (eq t (maxwell/json:get-key
                   (maxwell::chat-request-object (maxwell:make-client :model "m")
                                                 #() #())
                   "stream")))
    (check (maxwell::sse-body-p (format nil "data: {}~%~%")))
    (check (not (maxwell::sse-body-p "{\"choices\":[]}")))
    (check (not (maxwell::sse-body-p "")))))

(defun test-request-stats ()
  ;; A measured request records latency, total time, and the tokens the
  ;; provider reported; a streamed reply carries its usage in the final chunk.
  (flet ((sse (&rest events)
           (with-output-to-string (out)
             (dolist (event events)
               (format out "data: ~A~%~%" event)))))
    (let* ((body (sse "{\"choices\":[{\"delta\":{\"content\":\"hi\"}}]}"
                      "{\"choices\":[],\"usage\":{\"prompt_tokens\":10,\"completion_tokens\":4,\"total_tokens\":14,\"completion_tokens_details\":{\"reasoning_tokens\":3}}}"
                      "[DONE]"))
           (response (maxwell::decode-chat-sse body)))
      (check (maxwell/json:get-key response "usage")))
    ;; The wire counts map onto a record, and the totals are derived.
    (let ((stat (maxwell::request-stat-begin "test-model" :chat-completions)))
      (maxwell::note-request-piece stat 5)
      (maxwell::finish-request-stat
        stat (maxwell/json:json-object
               "choices" (maxwell/json:json-array)
               "usage" (maxwell/json:json-object
                          "prompt_tokens" 100
                          "completion_tokens" 20
                          "total_tokens" 120
                          "completion_tokens_details"
                          (maxwell/json:json-object "reasoning_tokens" 5))))
      (check (= 100 (maxwell::request-stat-prompt-tokens stat)))
      (check (= 20 (maxwell::request-stat-completion-tokens stat)))
      (check (= 120 (maxwell::request-stat-total-tokens stat)))
      (check (= 5 (maxwell::request-stat-reasoning-tokens stat)))
      (check (not (maxwell::request-stat-estimated stat)))
      (check (eq stat (maxwell::latest-request-stat)))
      (check (search "test-model" (maxwell::request-stat-line stat)))
      (check (search "= 120 tokens" (maxwell::request-stat-line stat)))
      ;; Output tokens per second is over the answering time, past the first
      ;; piece; with a known latency and total it is exact.
      (let ((timed (maxwell::request-stat-begin "m" :chat-completions)))
        (setf (maxwell::request-stat-latency timed) 1.0
              (maxwell::request-stat-total timed) 3.0)
        (maxwell::request-stat-take-usage
          timed (maxwell/json:json-object "completion_tokens" 10))
        (maxwell::request-stat-finish-tokens timed)
        (check (= 5.0 (maxwell::request-stat-tokens-per-second timed)))))
    ;; No usage: the output tokens are estimated from the reply's size.
    (let ((stat (maxwell::request-stat-begin "m" :chat-completions)))
      (maxwell::finish-request-stat
        stat (maxwell/json:json-object
               "choices"
               (maxwell/json:json-array
                 (maxwell/json:json-object
                   "message" (maxwell/json:json-object
                                "role" "assistant"
                                "content" "abcdefgh")))))
      (check (maxwell::request-stat-estimated stat))
      (check (= 2 (maxwell::request-stat-completion-tokens stat)))
      (check (= 2 (maxwell::request-stat-total-tokens stat))))
    ;; The summary is tokens weighted: completion tokens over answering time
    ;; across every request, so a slow request with many tokens weighs more
    ;; than the mean of the per-request rates.
    (let ((fast (maxwell::make-request-stat
                  :model "m" :tokens-per-second 100.0
                  :latency 0.5 :total 1.5 :completion-tokens 100
                  :total-tokens 200))
          (slow (maxwell::make-request-stat
                  :model "m" :tokens-per-second 10.0
                  :latency 1.0 :total 11.0 :completion-tokens 100
                  :total-tokens 200)))
      ;; 200 tokens over 1.0s + 10.0s = 18.2 tok/s, not the mean rate 55.
      (check (search "18.2 tok/s" (maxwell::request-stats-summary (list fast slow))))
      (check (not (search "55.0 tok/s" (maxwell::request-stats-summary (list fast slow)))))
      (check (search "2 requests" (maxwell::request-stats-summary (list fast slow)))))
    ;; Records older than the retention time are dropped; recent ones are kept.
    (let* ((now (get-universal-time))
           (old (maxwell::make-request-stat :model "m" :started
                                            (- now (* 31 24 60 60))))
           (fresh (maxwell::make-request-stat :model "m" :started (- now 60))))
      (let ((maxwell::*request-stats* (list fresh old)))
        (maxwell::expire-request-stats :time now)
        (check (equal (list fresh) maxwell::*request-stats*))))
    (check (= 30 maxwell::*request-stats-retention-days*))
    ;; The report reads over whatever has been recorded.
    (check (search "request" (maxwell::request-stats-report)))
    (check (search "Summary:" (maxwell::request-stats-report)))))

(defun run-tests ()
  ;; Tests may be run by the live agent through COMMON_LISP.
  (let ((maxwell:*agent* nil)
        ;; Nor touch the live session's transcript or display settings.
        (maxwell::*session-entries* (maxwell::make-entry-vector))
        (maxwell::*tool-display* :summary)
        (maxwell::*live-reasoning* nil)
        (maxwell::*status-bar-extra-rows* 0)
        (maxwell::*status-bar-drawn-rows* '())
        (maxwell::*reasoning-delta-function* nil)
        (maxwell::*request-stats* '())
        (maxwell::*older-entries-function* nil)
        (maxwell::*color* nil)
        (maxwell::*pending-image* nil)
        ;; Nor the live chat, its lock, or its image.
        (maxwell:*chat* (maxwell::make-chat))
        (maxwell::*checkpoint-path* nil)
        (maxwell::*lock-fd* nil)
        (maxwell::*busy-text* nil)
        (maxwell::*prompt-history* '())
        (maxwell::*prompt-kill* "")
        ;; Never touch the real remembered session while testing.
        (maxwell::*session-file*
          (merge-pathnames "maxwell-test-session-unused.json"
                           (uiop:temporary-directory)))
        (maxwell::*safeai-auth-file*
          (merge-pathnames "maxwell-test-safeai-unused.json"
                           (uiop:temporary-directory))))
    (setf *failures* 0)
    (test-json)
    (test-json-key-case)
    (test-eval)
    (test-vendored-libraries)
    (test-agent-loop)
    (test-tool-result-clipping)
    (test-nested-agent-run)
    (test-responses-adapter)
    (test-responses-sse)
    (test-reasoning-streams)
    (test-request-stats)
    (test-auth-encoding)
    (test-openai-client-contract)
    (test-model-catalog)
    (test-provider-switching)
    (test-review-regressions)
    (test-session-memory)
    (test-safeai-credentials)
    (test-safeai-key-checks)
    (test-unavailable-model-messages)
    (test-private-credential-header)
    (test-provider-presets)
    (test-steering-input)
    (test-steering-run)
    (test-turn-assembly)
    (test-turn-logging)
    (test-summary-wait)
    (test-log-paging)
    (test-image-paths-and-lock)
    (test-chat-transplant)
    (test-startup-migration)
    (test-command-line)
    (test-ui-queue)
    (test-workers)
    (test-interrupt-flag)
    (test-threaded-turn)
    (test-steering-shared-queue)
    (test-compactor-worker)
    (test-checkpoint-needs-ui-thread)
    (test-async-command)
    (test-terminal)
    (test-prompt-editing)
    (test-prompt-history)
    (run-ui-tests)
    (run-memory-tests)
    (format t "~&~:[~D Maxwell test(s) failed~;All Maxwell tests passed~].~%"
            (zerop *failures*) *failures*)
    (zerop *failures*)))


(defun test-review-regressions ()
  ;; A catalog lookup runs on a worker and can synchronously ask the UI to
  ;; service work without freezing it. No provider network access in this test.
  (let* ((maxwell::*ui-thread* (bt:current-thread))
         (maxwell::*auth-file* (temporary-test-path "no-openai"))
         (maxwell::*safeai-auth-file* (temporary-test-path "no-safeai"))
         (maxwell::*model-credential-environment* (constantly nil))
         (serviced nil)
         (maxwell::*model-lister*
           (lambda (client)
             (declare (ignore client))
             (check (not (maxwell::ui-thread-p)))
             (maxwell::ui-call (lambda () (setf serviced t)))
             '(("test-model" . "test-model"))))
         (agent (maxwell:make-agent :client (maxwell:make-client
                                            :auth-provider :openai :model "test-model"))))
    (check (search "test-model"
                   (with-output-to-string (*standard-output*)
                     (maxwell::run-model-command agent))))
    (check serviced)
    (check (null (maxwell::live-workers))))

  ;; Functions returned by UI work are data, not exception trampolines.
  (let* ((maxwell::*ui-thread* (bt:current-thread))
         (fn (lambda () (error "must not execute")))
         (result nil)
         (worker (maxwell::start-worker
                   "function result"
                   (lambda ()
                     (setf result
                           (list (multiple-value-list (maxwell::ui-call (lambda () fn)))
                                 (multiple-value-list
                                   (maxwell::ui-call (lambda () (values fn :second)))))))
                   :bindings (list (cons 'maxwell::*ui-thread* maxwell::*ui-thread*)))))
    (maxwell::worker-join worker)
    (check (eq fn (first (first result))))
    (check (equal (list fn :second) (second result))))
  ;; All subprocess and credential files start private, even under umask 0.
  (let* ((old-mask (sb-posix:umask 0))
         (path (temporary-test-path "private"))
         (job nil))
    (unwind-protect
         (progn
           (maxwell::write-private-json-file (maxwell/json:json-object "key" "test") path)
           (check (= #o600 (logand #o777 (sb-posix:stat-mode (sb-posix:stat path)))))
           (setf job (maxwell::start-command '("/bin/cat") :input "private conversation"))
           (loop until (maxwell::command-finished-p job) do (sleep 0.01))
           (dolist (file (list (maxwell::command-job-input-path job)
                              (maxwell::command-job-output-path job)
                              (maxwell::command-job-error-path job)))
             (check (= #o600 (logand #o777 (sb-posix:stat-mode (sb-posix:stat file))))))
           (check (string= "private conversation" (maxwell::finish-command job)))
           (setf job nil))
      (when job (maxwell::cancel-command job))
      (when (probe-file path) (delete-file path))
      (sb-posix:umask old-mask)))
  ;; The final output must reach WATCH even when the child exits between polls.
  (let ((seen "") (maxwell::*command-poll-interval* 0.1))
    (multiple-value-bind (output err code)
        (maxwell::run-command '("/bin/echo" "last chunk")
                              :watch (lambda (chunk) (setf seen (concatenate 'string seen chunk))))
      (declare (ignore err))
      (check (zerop code))
      (check (string= output seen))))
  ;; SSE framing, role-only deltas and usage events are not model tokens.
  (dolist (wire '(:chat-completions :responses))
    (let* ((stat (maxwell::request-stat-begin "test" wire))
           (reader (maxwell::%make-sse-reader))
           (handler (maxwell::request-sse-handler stat wire)))
      (maxwell::sse-reader-feed reader (format nil ": keep-alive~%~%") handler)
      (funcall handler nil "{\"choices\":[{\"delta\":{\"role\":\"assistant\"}}]}")
      (funcall handler nil "{\"type\":\"response.created\"}")
      (funcall handler nil "{\"choices\":[],\"usage\":{}}")
      (check (null (maxwell::request-stat-first-real stat)))
      (funcall handler nil
               (if (eq wire :responses)
                   "{\"type\":\"response.output_text.delta\",\"delta\":\"x\"}"
                   "{\"choices\":[{\"delta\":{\"content\":\"x\"}}]}"))
      (check (maxwell::request-stat-first-real stat)))))
