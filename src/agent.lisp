(in-package #:maxwell)

(defun normalize-directory (directory)
  (uiop:ensure-directory-pathname (truename directory)))

(defun make-agent (&key (client (make-client))
                         (workspace (uiop:getcwd))
                         (system-prompt *base-system-prompt*)
                         model-function)
  (%make-agent :client client
               :workspace (normalize-directory workspace)
               :system-prompt system-prompt
               :model-function model-function))

(defun common-lisp-tool ()
  (json-object
    "type" "function"
    "function"
    (json-object
      "name" "common_lisp"
      "description"
      "Evaluate one or more Common Lisp forms in the persistent MAXWELL/WORKSPACE package. Standard output and the final form's values are returned. The working directory is the project root. Definitions, loaded systems, and mutations remain available to later calls."
      "parameters"
      (json-object
        "type" "object"
        "properties" (json-object
                       "code" (json-object
                                "type" "string"
                                "description" "Common Lisp source forms to evaluate."))
        "required" (json-array "code")
        "additionalProperties" nil))))

(defun wire-messages (agent)
  (cons (json-object "role" "system" "content" (agent-system-prompt agent))
        (agent-history agent)))

(defun assistant-message (response)
  (let* ((choices (get-key response "choices"))
         (choice (and (vectorp choices) (plusp (length choices)) (aref choices 0)))
         (message (and choice (get-key choice "message"))))
    (unless (hash-table-p message)
      (error 'provider-error :message "Provider response has no assistant message."))
    message))

(defun invoke-model (agent)
  (if (agent-model-function agent)
      (funcall (agent-model-function agent) (wire-messages agent)
               (json-array (common-lisp-tool)))
      (call-provider (agent-client agent) (wire-messages agent)
                     (json-array (common-lisp-tool)))))

(defun tool-result-message (call result)
  (json-object "role" "tool"
               "tool_call_id" (get-key call "id")
               "content" result))

(defun execute-tool-call (agent call)
  (let* ((function (get-key call "function"))
         (name (get-key function "name"))
         (argument-value (get-key function "arguments" "{}")))
    (cond
      ((not (string= name "common_lisp"))
       (tool-result-message call (format nil "Unknown tool ~S." name)))
      (t
       (handler-case
           (let* ((arguments (if (stringp argument-value)
                                 (decode argument-value)
                                 argument-value))
                  (code (get-key arguments "code")))
             (unless (stringp code)
               (error "COMMON_LISP requires a string CODE argument."))
             (format t "~&[common-lisp]~%~A~%" code)
             (let ((result (eval-lisp code :directory (agent-workspace agent))))
               (format t "~A~%" result)
               (tool-result-message call result)))
         (error (condition)
           (tool-result-message call
                                (format nil "Tool argument error: ~A" condition))))))))

(defun message-content (message)
  (let ((content (get-key message "content")))
    (if (stringp content) content "")))

(defun finish-pending-image ()
  (when *pending-image*
    (let ((path *pending-image*))
      (setf *pending-image* nil)
      (save-image path))))

(defun run (prompt &key (agent (or *agent* (make-agent))) (print-response t))
  "Run one user PROMPT through AGENT until the model stops calling Lisp."
  (setf *agent* agent)
  (setf (agent-history agent)
        (append (agent-history agent)
                (list (json-object "role" "user" "content" prompt))))
  (loop
    (let* ((response (invoke-model agent))
           (message (assistant-message response))
           (tool-calls (get-key message "tool_calls")))
      (setf (agent-history agent)
            (append (agent-history agent) (list message)))
      (if (and (vectorp tool-calls) (plusp (length tool-calls)))
          (loop for call across tool-calls
                do (setf (agent-history agent)
                         (append (agent-history agent)
                                 (list (execute-tool-call agent call)))))
          (let ((content (message-content message)))
            (when (and print-response (plusp (length content)))
              (format t "~&~A~%" content))
            (finish-pending-image)
            (return content))))))
