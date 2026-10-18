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

(defun trim-slashes (string)
  (string-right-trim "/" string))

(defun make-client (&key
                      (base-url (or (uiop:getenv "MAXWELL_BASE_URL")
                                    "https://api.openai.com/v1"))
                      (model (or (uiop:getenv "MAXWELL_MODEL") ""))
                      (compact-model (uiop:getenv "MAXWELL_COMPACT_MODEL"))
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
  (%make-client :base-url (trim-slashes base-url)
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
    (format nil "Provider request failed (HTTP/process status ~A): ~A~@[~%~A~]"
            status detail
            (and (plusp (length stderr)) stderr))))

(defun copy-object (object)
  (let ((copy (make-hash-table :test #'equal)))
    (when (hash-table-p object)
      (maphash (lambda (key value) (setf (gethash key copy) value)) object))
    copy))

(defun chat-message (message)
  "Copy MESSAGE while removing Maxwell's provider-private bookkeeping keys."
  (let ((copy (make-hash-table :test #'equal)))
    (maphash (lambda (key value)
               (unless (and (stringp key)
                            (>= (length key) 9)
                            (string= key "_maxwell_" :end1 9 :end2 9))
                 (setf (gethash key copy) value)))
             message)
    copy))

(defun chat-request-object (client messages tools)
  (let ((request (copy-object (client-request-fields client))))
    ;; Core agent state always wins over optional provider-specific fields.
    (setf (gethash "model" request) (client-model client)
          (gethash "messages" request) (map 'vector #'chat-message messages)
          (gethash "tools" request) tools)
    request))

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
    request))

(defun response-output-text (output)
  (with-output-to-string (stream)
    (loop for item across output
          when (string= (get-key item "type" "") "message")
            do (let ((content (get-key item "content")))
                 (when (vectorp content)
                   (loop for part across content
                         when (string= (get-key part "type" "") "output_text")
                           do (write-string (get-key part "text" "") stream)))))))

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
                               "content" (response-output-text output))))
    (setf (gethash "_maxwell_responses_output" message) output)
    (when calls
      (setf (gethash "tool_calls" message) (coerce calls 'vector)))
    (json-object "choices" (json-array (json-object "message" message)))))

(defun provider-url (client)
  (or (client-endpoint client)
      (format nil "~A/~A" (client-base-url client)
              (if (eq (client-wire-api client) :responses)
                  "responses"
                  "chat/completions"))))

(defun streaming-responses-request-p (client request)
  (and (eq (client-wire-api client) :responses)
       (eq (get-key request "stream") t)))

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
  (let ((streaming (streaming-responses-request-p client request))
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

(defun parse-provider-output (output error-output status streaming)
  "Decode the reply curl printed for a provider request, or signal a
PROVIDER-ERROR. STATUS is curl's exit status."
  (multiple-value-bind (response-body http-status) (split-http-status output)
    (unless (success-status-p status http-status)
      (error 'provider-error
             :message (response-error-message response-body
                                              (if (zerop status) http-status status)
                                              error-output)))
    (handler-case (if streaming
                      (decode-responses-sse response-body)
                      (decode response-body))
      (error (condition)
        (when (typep condition 'provider-error) (error condition))
        (error 'provider-error
               :message (format nil "Provider returned invalid ~A: ~A~%~A"
                                (if streaming "SSE" "JSON")
                                condition response-body))))))

(defun post-provider (client request)
  (multiple-value-bind (arguments body streaming private-files)
      (provider-request-arguments client request)
    ;; The request body goes to the child's standard input. At the prompt the
    ;; child runs asynchronously so the user can still browse the transcript
    ;; and queue steering prompts while the model works.
    (multiple-value-bind (output error-output status)
        (run-command arguments :input body :private-files private-files)
      (parse-provider-output output error-output status streaming))))

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
                                           (provider-job-streaming job)))))
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
