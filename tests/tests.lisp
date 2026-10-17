(in-package #:maxwell/tests)

(defvar *failures* 0)

(defmacro check (form)
  `(unless ,form
     (incf *failures*)
     (format *error-output* "FAIL: ~S~%" ',form)))

(defun fake-response (&key content tool-calls)
  (maxwell/json:json-object
    "choices"
    (maxwell/json:json-array
      (maxwell/json:json-object
        "message"
        (let ((message (maxwell/json:json-object "role" "assistant"
                                                 "content" (or content maxwell/json:+null+))))
          (when tool-calls (setf (gethash "tool_calls" message) tool-calls))
          message)))))

(defun tool-call (code)
  (maxwell/json:json-object
    "id" "call-1"
    "type" "function"
    "function" (maxwell/json:json-object
                  "name" "common_lisp"
                  "arguments" (maxwell/json:encode
                                 (maxwell/json:json-object "code" code)))))

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
  (check (string= "42" (maxwell:eval-lisp "(1+ *remembered*)"))))

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

(defun run-tests ()
  (setf *failures* 0)
  (test-json)
  (test-json-key-case)
  (test-eval)
  (test-agent-loop)
  (test-responses-adapter)
  (test-responses-sse)
  (test-auth-encoding)
  (test-openai-client-contract)
  (test-model-catalog)
  (format t "~&~:[~D Maxwell test(s) failed~;All Maxwell tests passed~].~%"
          (zerop *failures*) *failures*)
  (zerop *failures*))
