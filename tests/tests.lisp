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
      (check (= 4 (length (maxwell:agent-history agent)))))))

(defun entries-text (entries)
  (format nil "~{~A~^~%~}" (map 'list #'maxwell::entry-text entries)))

(defun test-nested-agent-run ()
  (let* ((outer (maxwell:make-agent))
         (maxwell:*agent* outer)
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
    (check (string= "Still here."
                    (maxwell:run "Continue." :print-response nil)))
    (check (= 6 (length (maxwell:agent-history outer)))))
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
  (let* ((requests '())
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
    (check (= 5 (length (maxwell:agent-history agent)))))
  ;; A prompt typed while the model is answering continues the turn.
  (let* ((calls 0)
         (model (lambda (messages tools)
                  (declare (ignore messages tools))
                  (incf calls)
                  (when (= calls 1) (maxwell::queue-steering "one more thing"))
                  (fake-response :content (format nil "reply ~D" calls))))
         (agent (maxwell:make-agent :model-function model)))
    (check (string= "reply 2" (maxwell:run "Go." :agent agent :print-response nil)))
    (check (= 2 calls))
    (check (= 4 (length (maxwell:agent-history agent))))
    (check (string= "one more thing"
                    (maxwell/json:get-key (third (maxwell:agent-history agent))
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

(defun run-tests ()
  ;; Tests may be run by the live agent through COMMON_LISP.
  (let ((maxwell:*agent* nil)
        ;; Nor touch the live session's transcript or display settings.
        (maxwell::*session-entries* (maxwell::make-entry-vector))
        (maxwell::*tool-display* :summary)
        (maxwell::*older-entries-function* nil)
        (maxwell::*color* nil)
        (maxwell::*pending-image* nil)
        (maxwell::*prompt-history* '())
        (maxwell::*prompt-kill* "")
        ;; Never touch the real remembered session while testing.
        (maxwell::*session-file*
          (merge-pathnames "maxwell-test-session-unused.json"
                           (uiop:temporary-directory))))
    (setf *failures* 0)
    (test-json)
    (test-json-key-case)
    (test-eval)
    (test-agent-loop)
    (test-nested-agent-run)
    (test-responses-adapter)
    (test-responses-sse)
    (test-auth-encoding)
    (test-openai-client-contract)
    (test-model-catalog)
    (test-session-memory)
    (test-steering-input)
    (test-steering-run)
    (test-async-command)
    (test-terminal)
    (test-prompt-editing)
    (test-prompt-history)
    (run-ui-tests)
    (run-memory-tests)
    (format t "~&~:[~D Maxwell test(s) failed~;All Maxwell tests passed~].~%"
            (zerop *failures*) *failures*)
    (zerop *failures*)))
