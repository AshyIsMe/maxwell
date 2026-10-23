(in-package #:maxwell)

(defstruct (client (:constructor %make-client))
  (base-url "https://api.openai.com/v1" :type string)
  endpoint
  (wire-api :chat-completions :type keyword)
  (model "" :type string)
  ;; The cheaper model that builds OptChat summaries; NIL means MODEL.
  compact-model
  (api-key-environment "OPENAI_API_KEY" :type string)
  auth-provider
  (extra-headers '() :type list)
  request-fields
  (timeout 300 :type integer))

(defparameter +effort-field+ "_maxwell_effort"
  "Where a client keeps the thinking effort /effort set, such as \"high\". It is
provider-private bookkeeping: a request sends the level in the provider's own
spelling instead, never this field.")

(defun copy-object (object)
  "A shallow copy of OBJECT, a JSON object, or an empty object when it is not
one. Nested objects are shared, so copy those too before changing them."
  (let ((copy (make-hash-table :test #'equal)))
    (when (hash-table-p object)
      (maphash (lambda (key value) (setf (gethash key copy) value)) object))
    copy))

(defun client-effort (client)
  "The thinking effort CLIENT asks its model for, or NIL for the provider's own
default."
  (get-key (client-request-fields client) +effort-field+))

(defun set-client-effort (client level)
  "Ask CLIENT's model for thinking LEVEL, or the provider's own default when
LEVEL is NIL. The level travels in the client's request fields, so a copy of the
client keeps it and no other request field is disturbed."
  (let ((fields (copy-object (client-request-fields client))))
    (if (and level (plusp (length level)))
        (setf (gethash +effort-field+ fields) level)
        (remhash +effort-field+ fields))
    (setf (client-request-fields client)
          (if (zerop (hash-table-count fields)) nil fields))
    (client-effort client)))

(defun trim-slashes (string)
  (string-right-trim "/" string))

(defun make-client (&key
                      (base-url (or (uiop:getenv "MAXWELL_BASE_URL")
                                    "https://api.openai.com/v1"))
                      (model (or (uiop:getenv "MAXWELL_MODEL") ""))
                      (compact-model (uiop:getenv "MAXWELL_COMPACT_MODEL"))
                      effort
                      (api-key-environment
                        (or (uiop:getenv "MAXWELL_API_KEY_ENV")
                            "OPENAI_API_KEY"))
                      auth-provider
                      (endpoint (uiop:getenv "MAXWELL_ENDPOINT"))
                      (wire-api (or (uiop:getenv "MAXWELL_WIRE_API")
                                    :chat-completions))
                      (extra-headers '())
                      (request-fields nil)
                      (timeout 300))
  (let* ((wire-api (if (keywordp wire-api)
                       wire-api
                       (intern (string-upcase wire-api) :keyword)))
         (auth-provider (if (or (null auth-provider) (keywordp auth-provider))
                            auth-provider
                            (intern (string-upcase auth-provider) :keyword))))
    (unless (member wire-api '(:chat :chat-completions :responses))
      (error "WIRE-API must be chat, chat-completions, or responses."))
    (when (eq auth-provider :openai)
      ;; ChatGPT-plan tokens may only go to the public Responses endpoint and
      ;; require the non-stored streaming request contract.
      (setf base-url +openai-resource+
            endpoint nil
            wire-api :responses)
      (let ((fields (let ((copy (make-hash-table :test #'equal)))
                      (when (hash-table-p request-fields)
                        (maphash (lambda (key value)
                                   (setf (gethash key copy) value))
                                 request-fields))
                      copy)))
        (setf (gethash "store" fields) nil
              (gethash "stream" fields) t
              request-fields fields)))
  (let ((client (%make-client
                  :base-url (trim-slashes base-url)
                  :endpoint endpoint
                  :wire-api (if (eq wire-api :chat) :chat-completions wire-api)
                  :model model
                  :compact-model (and (stringp compact-model)
                                      (plusp (length compact-model))
                                      compact-model)
                  :api-key-environment api-key-environment
                  :auth-provider auth-provider
                  :extra-headers extra-headers
                  :request-fields request-fields
                  :timeout timeout)))
    (when (and (stringp effort) (plusp (length effort)))
      (set-client-effort client effort))
    client)))

(defun configure-client-for-openai-oauth (client)
  (let ((fields (copy-object (client-request-fields client))))
    (setf (gethash "store" fields) nil
          (gethash "stream" fields) t
          (client-base-url client) +openai-resource+
          (client-endpoint client) nil
          (client-wire-api client) :responses
          (client-auth-provider client) :openai
          (client-request-fields client) fields))
  client)

(define-condition provider-error (error)
  ((message :initarg :message :reader provider-error-message))
  (:report (lambda (condition stream)
             (write-string (provider-error-message condition) stream))))

(defun response-error-message (body status stderr)
  (let ((detail
          (handler-case
              (let* ((json (decode body))
                     (error (get-key json "error")))
                (or (and (hash-table-p error) (get-key error "message")) body))
            (condition () body))))
    (if (equal status "429")
        (format nil "The provider is rate limiting requests (HTTP 429): ~A~@[~%~A~]~%~
Your key may have reached its quota, or the model may have no capacity left. Wait and try again, or pick another model with /model."
                detail (and (plusp (length stderr)) stderr))
        (format nil "Provider request failed (HTTP/process status ~A): ~A~@[~%~A~]"
                status detail
                (and (plusp (length stderr)) stderr)))))

(defun chat-message (message)
  "Copy MESSAGE while removing Maxwell's provider-private bookkeeping keys and
any reasoning the provider sent, which is shown but never sent back."
  (let ((copy (make-hash-table :test #'equal)))
    (maphash (lambda (key value)
               (unless (and (stringp key)
                            (or (and (>= (length key) 9)
                                     (string= key "_maxwell_" :end1 9 :end2 9))
                                (member key '("reasoning" "reasoning_content")
                                        :test #'string=)))
                 (setf (gethash key copy) value)))
             message)
    copy))

;;; Reasoning traces. A model that shows its thinking names it differently on
;;; each wire: the Responses API streams reasoning summaries, while
;;; Chat Completions providers send REASONING_CONTENT or REASONING. Maxwell
;;; carries whatever it receives under one private key on the assistant
;;; message, shows it, and never sends it back.

(defparameter +reasoning-key+ "_maxwell_reasoning"
  "Where an assistant message keeps the reasoning its provider sent.")

(defvar *reasoning-delta-function* nil
  "When non-NIL, a function called with each piece of the model's reasoning as
it streams in, so the prompt can show it while the model thinks.")

(defun note-reasoning-delta (text)
  "Tell the live trace about one streamed piece of TEXT, if any is listening."
  (when (and *reasoning-delta-function* (stringp text) (plusp (length text)))
    (funcall *reasoning-delta-function* text))
  text)

(defun message-reasoning (message)
  "The reasoning text MESSAGE carries, or NIL."
  (let ((text (get-key message +reasoning-key+)))
    (and (stringp text) (plusp (length text)) text)))

(defun normalize-message-reasoning (message)
  "Move any reasoning a provider put on MESSAGE under +REASONING-KEY+, so it
is shown and never sent back in the next request."
  (let ((text (or (get-key message "reasoning_content")
                  (get-key message "reasoning"))))
    (when (and (stringp text) (plusp (length text)))
      (setf (gethash +reasoning-key+ message) text))
    (remhash "reasoning_content" message)
    (remhash "reasoning" message))
  message)

(defun apply-effort (client request)
  "Send CLIENT's thinking effort in REQUEST's provider spelling: a reasoning
object on the Responses wire, REASONING_EFFORT on Chat Completions. Other fields
of a reasoning object the caller sent are kept."
  (let ((effort (get-key request +effort-field+)))
    (remhash +effort-field+ request)
    (when (and effort (plusp (length effort)))
      (if (eq (client-wire-api client) :responses)
          (let ((reasoning (get-key request "reasoning")))
            (if (hash-table-p reasoning)
                (let ((copy (copy-object reasoning)))
                  (setf (gethash "effort" copy) effort
                        (gethash "reasoning" request) copy))
                (setf (gethash "reasoning" request)
                      (json-object "effort" effort))))
          (setf (gethash "reasoning_effort" request) effort)))
    request))

(defun chat-request-object (client messages tools)
  (let ((request (copy-object (client-request-fields client))))
    ;; Core agent state always wins over optional provider-specific fields.
    ;; The reply is streamed so that a reasoning trace can be shown while the
    ;; model thinks; a provider that ignores STREAM still answers in one piece.
    (setf (gethash "model" request) (client-model client)
          (gethash "messages" request) (map 'vector #'chat-message messages)
          (gethash "tools" request) tools
          (gethash "stream" request) t
          ;; Ask for a usage report at the end of the stream, so token counts
          ;; are measured rather than estimated.
          (gethash "stream_options" request)
          (json-object "include_usage" t))
    (apply-effort client request)))

(defun response-tool (tool)
  (let ((function (get-key tool "function")))
    (json-object "type" "function"
                 "name" (get-key function "name")
                 "description" (get-key function "description")
                 "parameters" (get-key function "parameters"))))

(defun response-input (messages)
  (let ((items '()))
    (dolist (message (rest messages))
      (let ((role (get-key message "role")))
        (cond
          ((string= role "tool")
           (push (json-object "type" "function_call_output"
                              "call_id" (get-key message "tool_call_id")
                              "output" (get-key message "content"))
                 items))
          ((and (string= role "assistant")
                (vectorp (get-key message "_maxwell_responses_output")))
           (loop for item across (get-key message "_maxwell_responses_output")
                 do (push item items)))
          (t
           (push (json-object "role" role "content" (get-key message "content" ""))
                 items)))))
    (coerce (nreverse items) 'vector)))

(defun responses-request-object (client messages tools)
  (let ((request (copy-object (client-request-fields client))))
    (setf (gethash "model" request) (client-model client)
          (gethash "instructions" request) (get-key (first messages) "content")
          (gethash "input" request) (response-input messages)
          (gethash "tools" request) (map 'vector #'response-tool tools))
    (apply-effort client request)))

(defun response-output-text (output)
  (with-output-to-string (stream)
    (loop for item across output
          when (string= (get-key item "type" "") "message")
            do (let ((content (get-key item "content")))
                 (when (vectorp content)
                   (loop for part across content
                         when (string= (get-key part "type" "") "output_text")
                           do (write-string (get-key part "text" "") stream)))))))

(defun responses-reasoning-text (output)
  "The reasoning summaries carried by the Responses OUTPUT items, or an
empty string when there are none."
  (let ((text
          (with-output-to-string (stream)
            (loop for item across output
                  when (string= (get-key item "type" "") "reasoning")
                    do (let ((summary (get-key item "summary")))
                         (when (vectorp summary)
                           (loop for part across summary
                                 when (string= (get-key part "type" "")
                                               "summary_text")
                                   do (write-string (get-key part "text" "") stream)
                                      (write-string (format nil "~%~%") stream))))))))
    (string-trim '(#\Space #\Tab #\Newline #\Return) text)))

(defun responses-as-chat-response (response)
  (let* ((output (get-key response "output" #()))
         (calls
           (loop for item across output
                 when (string= (get-key item "type" "") "function_call")
                   collect (json-object
                             "id" (get-key item "call_id" (get-key item "id"))
                             "type" "function"
                             "function" (json-object
                                          "name" (get-key item "name")
                                          "arguments" (get-key item "arguments" "{}")))))
         (message (json-object "role" "assistant"
                               "content" (response-output-text output)))
         (reasoning (responses-reasoning-text output)))
    (setf (gethash "_maxwell_responses_output" message) output)
    (when (plusp (length reasoning))
      (setf (gethash +reasoning-key+ message) reasoning))
    (when calls
      (setf (gethash "tool_calls" message) (coerce calls 'vector)))
    (let ((chat (json-object "choices"
                             (json-array (json-object "message" message)))))
      ;; Keep the Responses usage so the request can be measured like a
      ;; Chat Completions one.
      (let ((usage (get-key response "usage")))
        (when (hash-table-p usage) (setf (gethash "usage" chat) usage)))
      chat)))

(defun provider-url (client)
  (or (client-endpoint client)
      (format nil "~A/~A" (client-base-url client)
              (if (eq (client-wire-api client) :responses)
                  "responses"
                  "chat/completions"))))

(defun stream-requested-p (request)
  "True when REQUEST asks the provider to stream its reply."
  (eq (get-key request "stream") t))

(defun sse-body-p (body)
  "True when BODY looks like a Server-Sent Events stream rather than JSON.
The first line may be a comment (a lone colon, as in a keep-alive), which is
skipped before the first data or event field is looked for."
  (loop with start = 0
        for end = (position #\Newline body :start start)
        for line = (strip-trailing-return
                     (string-trim '(#\Space #\Tab #\Return)
                                  (subseq body start end)))
        for colon = (position #\: line)
        do (cond ((zerop (length line)) nil)
                 ((and colon (zerop colon)) nil)
                 ((and colon (member (subseq line 0 colon) '("data" "event")
                                     :test #'string=))
                  (return t))
                 (t (return nil)))
        while end
        do (setf start (1+ end))
        finally (return nil)))


(defun strip-trailing-return (line)
  (if (and (plusp (length line))
           (char= (char line (1- (length line))) #\Return))
      (subseq line 0 (1- (length line)))
      line))

(defun sse-field-value (line colon)
  (let ((start (1+ colon)))
    (when (and (< start (length line))
               (char= (char line start) #\Space))
      (incf start))
    (subseq line start)))

(defun sse-failure-detail (event)
  (let* ((response (get-key event "response"))
         (error-object (or (get-key event "error")
                           (and (hash-table-p response)
                                (get-key response "error")))))
    (or (and (hash-table-p error-object)
             (get-key error-object "message"))
        (get-key event "message")
        (and (hash-table-p response) (get-key response "status"))
        "unknown streaming error")))

(defun decode-responses-sse (body)
  "Return the complete Response carried by a Responses API SSE stream."
  (let ((event-name nil)
        (data-lines '())
        (completed-response nil)
        (failure nil)
        (output-items (make-hash-table))
        (highest-output-index -1))
    (labels ((dispatch-event ()
               (when data-lines
                 (let ((data (format nil "~{~A~^~%~}" (nreverse data-lines))))
                   (setf data-lines '())
                   (unless (string= data "[DONE]")
                     (handler-case
                         (let* ((event (decode data))
                                (type (or (get-key event "type") event-name)))
                           (cond
                             ((string= type "response.output_item.done")
                              (let ((index (get-key event "output_index"))
                                    (item (get-key event "item")))
                                (when (and (integerp index) (hash-table-p item))
                                  (setf (gethash index output-items) item
                                        highest-output-index
                                        (max highest-output-index index)))))
                             ((string= type "response.completed")
                              (let ((response (get-key event "response")))
                                (unless (hash-table-p response)
                                  (error "response.completed has no response object"))
                                ;; ChatGPT-plan requests use store:false, so the
                                ;; terminal response can omit output. Rebuild it
                                ;; from the completed output-item events.
                                (let ((output (get-key response "output")))
                                  (when (and (plusp (hash-table-count output-items))
                                             (or (not (vectorp output))
                                                 (zerop (length output))))
                                    (setf (gethash "output" response)
                                          (coerce
                                            (loop for index from 0
                                                  to highest-output-index
                                                  for item = (gethash index output-items)
                                                  when item collect item)
                                            'vector))))
                                (setf completed-response response)))
                             ((member type '("response.failed" "response.incomplete" "error")
                                      :test #'string=)
                              (setf failure
                                    (format nil "~A: ~A" type
                                            (sse-failure-detail event))))))
                       (error (condition)
                         (error 'provider-error
                                :message
                                (format nil "Invalid Responses SSE event: ~A~%~A"
                                        condition data)))))))
               (setf event-name nil)))
      (with-input-from-string (stream body)
        (loop for raw-line = (read-line stream nil nil)
              while raw-line
              for line = (strip-trailing-return raw-line)
              do (cond
                   ((zerop (length line)) (dispatch-event))
                   ((char= (char line 0) #\:))
                   (t
                    (let ((colon (position #\: line)))
                      (when colon
                        (let ((field (subseq line 0 colon))
                              (value (sse-field-value line colon)))
                          (cond
                            ((string= field "event") (setf event-name value))
                            ((string= field "data") (push value data-lines)))))))))
        (dispatch-event)))
    (cond
      (completed-response completed-response)
      (failure
       (error 'provider-error
              :message (format nil "Provider Responses stream failed: ~A" failure)))
      (t
       (error 'provider-error
              :message "Provider Responses stream ended without response.completed.")))))

;;; A streamed reply arrives in pieces, so the reasoning can be shown while the
;;; model is still thinking. SSE-READER-FEED parses whatever has arrived so
;;; far; once the stream ends the whole reply is parsed again, in one piece, by
;;; DECODE-CHAT-SSE or DECODE-RESPONSES-SSE.

(defstruct (sse-reader (:constructor %make-sse-reader))
  "A Server-Sent Events stream read in pieces: the tail of an unfinished line
and the event being assembled."
  (rest "")
  (event nil)
  (data '()))

(defun sse-reader-feed (reader chunk handler)
  "Read CHUNK, the newest piece of a stream, calling HANDLER with the name and
data of each complete event."
  (let* ((text (concatenate 'string (sse-reader-rest reader) chunk))
         (lines (uiop:split-string text :separator '(#\Newline)))
         (complete (butlast lines)))
    (setf (sse-reader-rest reader) (or (car (last lines)) ""))
    (dolist (raw complete)
      (let ((line (strip-trailing-return raw)))
        (cond
          ((zerop (length line))
           (when (sse-reader-data reader)
             (funcall handler (sse-reader-event reader)
                      (format nil "~{~A~^~%~}" (nreverse (sse-reader-data reader))))
             (setf (sse-reader-data reader) '()
                   (sse-reader-event reader) nil)))
          ((char= (char line 0) #\:))
          (t
           (let ((colon (position #\: line)))
             (when colon
               (let ((field (subseq line 0 colon))
                     (value (sse-field-value line colon)))
                 (cond ((string= field "event")
                        (setf (sse-reader-event reader) value))
                       ((string= field "data")
                        (push value (sse-reader-data reader)))))))))))))

(defun responses-sse-reasoning-handler (name data)
  "Show the reasoning that a Responses SSE event carries."
  (declare (ignore name))
  (handler-case
      (let* ((event (decode data))
             (type (get-key event "type")))
        (when (and (stringp type)
                   (member type '("response.reasoning_summary_text.delta"
                                  "response.reasoning_text.delta")
                           :test #'string=))
          (note-reasoning-delta (get-key event "delta"))))
    (error () nil)))

(defun chat-sse-reasoning-handler (name data)
  "Show the reasoning that a Chat Completions SSE event carries."
  (declare (ignore name))
  (handler-case
      (let* ((event (decode data))
             (choices (get-key event "choices"))
             (delta (and (vectorp choices) (plusp (length choices))
                         (get-key (aref choices 0) "delta"))))
        (when (hash-table-p delta)
          (note-reasoning-delta (or (get-key delta "reasoning_content")
                                    (get-key delta "reasoning")))))
    (error () nil)))

(defun sse-reasoning-handler (wire-api)
  "The function that shows the reasoning a WIRE-API stream carries."
  (if (eq wire-api :responses)
      #'responses-sse-reasoning-handler
      #'chat-sse-reasoning-handler))

(defun decode-chat-sse (body)
  "Return the Chat Completions response carried by a streaming BODY, rebuilt
from its deltas: the content, the reasoning, and the tool calls."
  (let ((content (make-string-output-stream))
        (reasoning (make-string-output-stream))
        (calls (make-hash-table))
        (highest -1)
        (usage nil)
        (failure nil))
    (labels ((merge-call (call)
               "Add one streamed tool-call delta to CALLS, by index."
               (let* ((index (or (get-key call "index") 0))
                      (existing (gethash index calls)))
                 (unless existing
                   (setf existing (json-object
                                    "id" (get-key call "id")
                                    "type" "function"
                                    "function" (json-object "name" ""
                                                             "arguments" ""))
                         (gethash index calls) existing
                         highest (max highest index)))
                 (let ((function (get-key call "function"))
                       (target (get-key existing "function")))
                   (when (hash-table-p function)
                     (let ((name (get-key function "name"))
                           (arguments (get-key function "arguments")))
                       (when (and (stringp name) (plusp (length name))
                                  (zerop (length (get-key target "name"))))
                         (setf (gethash "name" target) name))
                       (when (stringp arguments)
                         (setf (gethash "arguments" target)
                               (concatenate 'string
                                            (get-key target "arguments")
                                            arguments))))))))
             (handle-event (event)
               "Fold one decoded stream event into the reply being rebuilt."
               (let ((error-object (get-key event "error"))
                     (choices (get-key event "choices")))
                 ;; The closing chunk carries the usage; it may have no choices.
                 (let ((seen-usage (get-key event "usage")))
                   (when (hash-table-p seen-usage) (setf usage seen-usage)))
                 (when (hash-table-p error-object)
                   (setf failure (or (get-key error-object "message")
                                     "stream error")))
                 (when (and (vectorp choices) (plusp (length choices)))
                   (let* ((choice (aref choices 0))
                          (delta (get-key choice "delta")))
                     (when (hash-table-p delta)
                       (let ((text (get-key delta "content"))
                             (thought (or (get-key delta "reasoning_content")
                                          (get-key delta "reasoning")))
                             (tool-calls (get-key delta "tool_calls")))
                         (when (stringp text) (write-string text content))
                         (when (stringp thought) (write-string thought reasoning))
                         (when (vectorp tool-calls)
                           (loop for call across tool-calls do (merge-call call))))))))))
      (sse-reader-feed
        (%make-sse-reader) body
        (lambda (name data)
          (declare (ignore name))
          (unless (string= data "[DONE]")
            (let ((event (ignore-errors (decode data))))
              (when (hash-table-p event) (handle-event event)))))))
    (when failure
      (error 'provider-error
             :message (format nil "Provider stream failed: ~A" failure)))
    (let* ((thought (get-output-stream-string reasoning))
           (message (json-object "role" "assistant"
                                 "content" (get-output-stream-string content)))
           (tool-calls (loop for index from 0 to highest
                             for call = (gethash index calls)
                             when call collect call)))
      (when (plusp (length thought))
        (setf (gethash +reasoning-key+ message) thought))
      (when tool-calls
        (setf (gethash "tool_calls" message) (coerce tool-calls 'vector)))
      (let ((response (json-object "choices"
                                   (json-array (json-object "message" message)))))
        (when usage (setf (gethash "usage" response) usage))
        response))))


(defun client-authorization-token (client)
  (case (client-auth-provider client)
    (:openai (openai-access-token))
    (:safeai (safeai-api-key))
    (otherwise (uiop:getenv (client-api-key-environment client)))))

(defun bearer-header-arguments (token)
  "The curl arguments that send TOKEN as a bearer credential, and the private
files they read. The header goes through an owner-only file rather than the
command line, where any local user could read it with ps."
  (cond
    ((or (null token) (zerop (length token)))
     (values '() '()))
    ((find-if (lambda (character) (member character '(#\Return #\Newline))) token)
     ;; A line break would let the credential inject further headers.
     (error 'provider-error :message "The API credential contains a line break."))
    (t
     (let ((path (private-temporary-path "header")))
       (values (list "--header" (format nil "@~A" (namestring path)))
               (list (cons path (format nil "Authorization: Bearer ~A~%" token))))))))

(defun split-http-status (output)
  "Split curl OUTPUT, written with --write-out \"\\n%{http_code}\", into the
response body and the three-character HTTP status (\"000\" when none)."
  (let ((newline (position #\Newline output :from-end t)))
    (values (if newline (subseq output 0 newline) output)
            (if newline (subseq output (1+ newline)) "000"))))

(defun success-status-p (curl-status http-status)
  (and (zerop curl-status)
       (= (length http-status) 3)
       (char= (char http-status 0) #\2)))

(defun model-catalog-from-response (response)
  (let ((openai-models (get-key response "models"))
        (compatible-models (get-key response "data")))
    (cond
      ((vectorp openai-models)
       (loop for model across openai-models
             for slug = (get-key model "slug")
             for visibility = (get-key model "visibility")
             when (and (stringp slug)
                       (or (null visibility) (string= visibility "list")))
               collect (cons slug (get-key model "display_name" slug))))
      ((vectorp compatible-models)
       (loop for model across compatible-models
             for id = (get-key model "id")
             when (stringp id) collect (cons id id)))
      (t
       (error 'provider-error
              :message "Provider model response has neither a models nor data array.")))))

(defun fetch-models-response (base-url token &key extra-headers (timeout 60))
  "GET BASE-URL/models, authorized by TOKEN when it is non-NIL. Returns the
response body, the HTTP status as a string (\"000\" when curl failed), curl's
exit status, and curl's error output."
  (multiple-value-bind (authorization private-files) (bearer-header-arguments token)
    (multiple-value-bind (output error-output status)
        (run-command
          (append (list "curl" "--silent" "--show-error" "--location"
                        "--max-time" (write-to-string timeout))
                  authorization
                  (loop for header in extra-headers
                        append (list "--header" header))
                  (list "--write-out" (format nil "~%%{http_code}")
                        (format nil "~A/models" (trim-slashes base-url))))
          :private-files private-files)
      (multiple-value-bind (body http-status) (split-http-status output)
        (values body http-status status error-output)))))

(defun model-catalog-from-body (body)
  "The (ID . DISPLAY-NAME) pairs in the model catalog JSON BODY."
  (handler-case (model-catalog-from-response (decode body))
    (provider-error (condition) (error condition))
    (error (condition)
      (error 'provider-error
             :message (format nil "Provider returned an invalid model catalog: ~A"
                              condition)))))

(defun list-provider-models (client)
  "Return available models as (ID . DISPLAY-NAME) pairs in provider order."
  (multiple-value-bind (body http-status status error-output)
      (fetch-models-response (client-base-url client)
                             (client-authorization-token client)
                             :extra-headers (client-extra-headers client)
                             :timeout (client-timeout client))
    (unless (success-status-p status http-status)
      (error 'provider-error
             :message (response-error-message body
                                              (if (zerop status) http-status status)
                                              error-output)))
    (model-catalog-from-body body)))

(defun provider-request-arguments (client request)
  "The curl arguments and request body that POST REQUEST to CLIENT's provider.
The third value is true when the reply is a Responses API event stream; the
fourth lists the private files, holding the credential, that the arguments read."
  (let ((streaming (stream-requested-p request))
        (url (provider-url client)))
    (multiple-value-bind (authorization private-files)
        (bearer-header-arguments (client-authorization-token client))
      (values
        (append (list "curl" "--silent" "--show-error" "--location"
                      "--max-time" (write-to-string (client-timeout client))
                      "--header" "Content-Type: application/json")
                (when streaming
                  (list "--no-buffer" "--header" "Accept: text/event-stream"))
                authorization
                (loop for header in (client-extra-headers client)
                      append (list "--header" header))
                (list "--data-binary" "@-"
                      "--write-out" (format nil "~%%{http_code}")
                      url))
        (encode request)
        streaming
        private-files))))

(defun parse-provider-output (output error-output status streaming wire-api)
  "Decode the reply curl printed for a provider request, or signal a
PROVIDER-ERROR. STATUS is curl's exit status; STREAMING says the reply was
asked for as a stream, and WIRE-API which stream it is."
  (multiple-value-bind (response-body http-status) (split-http-status output)
    (unless (success-status-p status http-status)
      (error 'provider-error
             :message (response-error-message response-body
                                              (if (zerop status) http-status status)
                                              error-output)))
    (let ((streamed-p (and streaming (sse-body-p response-body))))
      (handler-case
          (cond (streamed-p
                 (if (eq wire-api :responses)
                     (decode-responses-sse response-body)
                     (decode-chat-sse response-body)))
                (t (decode response-body)))
        (error (condition)
          (when (typep condition 'provider-error) (error condition))
          (error 'provider-error
                 :message (format nil "Provider returned invalid ~A: ~A~%~A"
                                  (if streamed-p "SSE" "JSON")
                                  condition response-body)))))))

(defparameter +model-check-statuses+ '("400" "404" "422" "429")
  "HTTP statuses a provider may answer when the requested model is gone. The
catalog is consulted only after one of these, so a healthy request costs nothing.")

(defun unlisted-model-note (client models)
  "A sentence saying CLIENT's model is missing from MODELS, the (ID . NAME)
pairs of the provider's catalog, or NIL when it is listed or MODELS is empty."
  (let ((model (client-model client)))
    (when (and models (not (find model models :key #'car :test #'string=)))
      (format nil "The model ~A is not in ~A's model list, so it may have been withdrawn. Available: ~{~A~^, ~}. Choose one with /model."
              model (client-base-url client) (mapcar #'car models)))))

(defun note-unlisted-model (client output condition)
  "CONDITION, a failed request's PROVIDER-ERROR, extended with a note when the
HTTP status in curl OUTPUT suggests a bad model and the catalog lacks it."
  (let* ((http-status (nth-value 1 (split-http-status output)))
         (models (and (member http-status +model-check-statuses+ :test #'string=)
                      (ignore-errors
                        (multiple-value-bind (body catalog-status curl-status)
                            (fetch-models-response (client-base-url client)
                                                   (client-authorization-token client)
                                                   :extra-headers (client-extra-headers client)
                                                   :timeout 10)
                          (and (success-status-p curl-status catalog-status)
                               (model-catalog-from-body body))))))
         (note (unlisted-model-note client models)))
    (if note
        (make-condition 'provider-error
                        :message (format nil "~A~%~A"
                                         (provider-error-message condition) note))
        condition)))

(defun sse-generated-output-p (wire-api name data)
  "True only for generated text, reasoning, or tool output, not SSE framing."
  (let ((event (ignore-errors (decode data))))
    (when (hash-table-p event)
      (if (eq wire-api :responses)
          (let ((type (or (get-key event "type") name))
                (delta (get-key event "delta")))
            (or (and (member type '("response.output_text.delta"
                                   "response.reasoning_summary_text.delta"
                                   "response.reasoning_text.delta"
                                   "response.function_call_arguments.delta")
                             :test #'equal)
                     (stringp delta) (plusp (length delta)))
                (and (equal type "response.output_item.added")
                     (equal (get-key (get-key event "item") "type")
                            "function_call"))))
          (let ((choices (get-key event "choices")))
            (and (vectorp choices)
                 (loop for choice across choices
                       for delta = (get-key choice "delta")
                       thereis
                       (or (loop for key in '("content" "reasoning_content" "reasoning")
                                 for text = (get-key delta key)
                                 thereis (and (stringp text) (plusp (length text))))
                           (let ((calls (get-key delta "tool_calls")))
                             (and (vectorp calls) (plusp (length calls))))))))))))

(defun request-sse-handler (stat wire-api)
  (let ((reasoning (sse-reasoning-handler wire-api)))
    (lambda (name data)
      (when (sse-generated-output-p wire-api name data)
        (note-request-piece stat 1))
      (funcall reasoning name data))))

(defun post-provider (client request)
  (multiple-value-bind (arguments body streaming private-files)
      (provider-request-arguments client request)
    ;; The request body goes to the child's standard input. At the prompt the
    ;; child runs asynchronously so the user can still browse the transcript
    ;; and queue steering prompts while the model works; a streamed reply is
    ;; shown as it arrives, so the model's reasoning appears while it thinks.
    (let* ((wire-api (client-wire-api client))
           (reader (and streaming (%make-sse-reader)))
           ;; Measure the request: the time to the first generated output, the
           ;; total time, and the token counts the provider reports.
           (stat (request-stat-begin (client-model client) wire-api))
           (handler (and reader (request-sse-handler stat wire-api))))
      (multiple-value-bind (output error-output status)
          (run-command arguments :input body :private-files private-files
                       :watch (and reader
                                   (lambda (chunk)
                                     (sse-reader-feed reader chunk handler))))
        (when reader (sse-reader-feed reader (format nil "~%~%") handler))
        (let ((response
                (handler-case
                    (parse-provider-output output error-output status streaming
                                           wire-api)
                  (provider-error (condition)
                    (error (note-unlisted-model client output condition))))))
          (finish-request-stat stat response)
          response)))))

(defun require-model (client)
  (when (zerop (length (client-model client)))
    (error 'provider-error
           :message "No model configured. Set MAXWELL_MODEL or pass --model.")))

(defun provider-request-object (client messages tools)
  (if (eq (client-wire-api client) :responses)
      (responses-request-object client messages tools)
      (chat-request-object client messages tools)))

;;; A provider request that runs in the background: the compactor starts
;;; several, polls them between keystrokes, and collects each when it ends.

(defstruct (provider-job (:constructor %make-provider-job))
  "An in-flight provider request started by START-PROVIDER-REQUEST."
  command
  wire-api
  streaming)

(defun start-provider-request (client messages tools)
  "Start a request for MESSAGES and TOOLS without waiting for the reply.
Returns a PROVIDER-JOB to poll with PROVIDER-REQUEST-FINISHED-P."
  (require-model client)
  (multiple-value-bind (arguments body streaming private-files)
      (provider-request-arguments client (provider-request-object client messages tools))
    (%make-provider-job :command (start-command arguments :input body
                                                          :private-files private-files)
                        :wire-api (client-wire-api client)
                        :streaming streaming)))

(defun provider-request-finished-p (job)
  "True once JOB's reply has fully arrived."
  (command-finished-p (provider-job-command job)))

(defun finish-provider-request (job)
  "Collect JOB's reply in Chat Completions form, as CALL-PROVIDER returns it.
Signals PROVIDER-ERROR when the request failed."
  (let ((response (multiple-value-bind (output error-output status)
                      (finish-command (provider-job-command job))
                    (parse-provider-output output error-output status
                                           (provider-job-streaming job)
                                           (provider-job-wire-api job)))))
    (if (eq (provider-job-wire-api job) :responses)
        (responses-as-chat-response response)
        response)))

(defun cancel-provider-request (job)
  "Abandon JOB, stopping its curl process."
  (cancel-command (provider-job-command job)))

(defun call-provider (client messages tools)
  (require-model client)
  (let ((response (post-provider client (provider-request-object client messages tools))))
    (if (eq (client-wire-api client) :responses)
        (responses-as-chat-response response)
        response)))
