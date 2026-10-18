(in-package #:maxwell)

;;; A turn. Each user message starts a fresh call of the model:
;;;
;;;   [tools] [system prompt] [view] [turn state + message]
;;;
;;; The system prompt and tools never change, and the view only grows at its
;;; end between merge batches, so consecutive turns share a long prompt-cache
;;; prefix. Everything said during the turn is logged in *CHAT* as it
;;; happens; the in-turn history, which keeps full tool output, is dropped
;;; when the turn ends.

(defun normalize-directory (directory)
  (uiop:ensure-directory-pathname (truename directory)))

(defun make-agent (&key (client (make-client))
                         (workspace (uiop:getcwd))
                         (system-prompt *base-system-prompt*)
                         model-function
                         compact-function)
  (%make-agent :client client
               :workspace (normalize-directory workspace)
               :system-prompt system-prompt
               :model-function model-function
               :compact-function compact-function))

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

;;; Logging

(defun log-entry (kind text &key (ok-p t) (show *show-entries*) (agent *agent*))
  "Log TEXT in *CHAT* as a KIND message from AGENT's workspace, and record it as
a transcript entry carrying its id. Returns the entry."
  (let ((ids (chat-append *chat* kind text :workspace (agent-workspace agent))))
    (record-entry (if (eq kind :maxwell) :assistant kind) text
                  :ok-p ok-p :id (first ids) :show show)))

(defun tool-result-message (call result)
  (json-object "role" "tool"
               "tool_call_id" (get-key call "id")
               "content" result))

(defun evaluate-tool-call (agent call)
  "Run CALL's code. Returns the result text and whether it succeeded."
  (let* ((function (get-key call "function"))
         (name (get-key function "name"))
         (argument-value (get-key function "arguments" "{}")))
    (if (not (string= name "common_lisp"))
        (values (format nil "Unknown tool ~S." name) nil)
        (handler-case
            (let* ((arguments (if (stringp argument-value)
                                  (decode argument-value)
                                  argument-value))
                   (code (get-key arguments "code")))
              (unless (stringp code)
                (error "COMMON_LISP requires a string CODE argument."))
              (multiple-value-bind (result error-p)
                  (eval-lisp code :directory (agent-workspace agent))
                (values result (not error-p))))
          (error (condition)
            (values (format nil "Tool argument error: ~A" condition) nil))))))

(defun execute-tool-call (agent call)
  "Run CALL and return its tool message. The call and its result are logged
and recorded as they happen; the message keeps the whole result, while the
log clips a long one."
  (log-entry :tool (tool-call-text call) :agent agent)
  (multiple-value-bind (result ok-p) (evaluate-tool-call agent call)
    (log-entry :echo result :ok-p ok-p :agent agent)
    (tool-result-message call result)))

(defun message-content (message)
  (let ((content (get-key message "content")))
    (if (stringp content) content "")))

(defun finish-pending-image ()
  (when *pending-image*
    (let ((path *pending-image*))
      (setf *pending-image* nil)
      ;; The snapshot ends the process, so the global chat is saved first.
      (when *checkpoint-path* (checkpoint))
      (save-image path))))

;;; Steering

(defvar *steering* '()
  "Prompts typed while a turn is running, oldest first. They reach the model
before its next request, so the user can redirect work already in progress.")

(defun queue-steering (text)
  "Record TEXT as a steering prompt for the model's next request."
  (when (plusp (length (string-trim '(#\Space #\Tab) text)))
    (setf *steering* (append *steering* (list text)))
    ;; The steering line already echoed it.
    (record-entry :steer text :show nil))
  text)

(defun steering-pending-p ()
  "True when a steering prompt is waiting to be delivered."
  (not (null *steering*)))

(defun inject-steering (agent)
  "Log queued steering prompts as :USER messages and append them to AGENT's
history."
  (let ((pending *steering*))
    (setf *steering* '())
    (dolist (text pending)
      (let ((id (first (chat-append *chat* :user text :workspace (agent-workspace agent))))
            (entry (find-if (lambda (entry)
                              (and (eq (entry-kind entry) :steer) (null (entry-id entry))
                                   (string= (entry-text entry) text)))
                            *session-entries* :from-end t)))
        (when entry (setf (entry-id entry) id))))
    (when pending
      (setf (agent-history agent)
            (append (agent-history agent)
                    (mapcar (lambda (text)
                              (json-object "role" "user" "content" text))
                            pending))))
    pending))

;;; Compaction and the wait for summaries

(defvar *busy-text* nil
  "When non-NIL, a short text, such as a spinner frame, that the steering line
shows before its prompt while a turn waits.")

(defparameter *summary-wait-limit* 600
  "The most seconds a turn waits for earlier messages to be summarized.")

(defun compactor-available-p (agent)
  "True when AGENT can make compaction requests: through its compact function,
or through its client when it has no fake model."
  (or (agent-compact-function agent) (null (agent-model-function agent))))

(defun tick-compactor (&optional (agent *agent*) (chat *chat*))
  "Advance CHAT's background compaction for AGENT without blocking. Requests
reuse the turns' system prompt and tools, so they share their cache prefix."
  (when (and agent chat)
    ;; Ctrl-C waits until the tick is over, so it never leaves the chat
    ;; halfway through an update.
    (sb-sys:without-interrupts
      (handler-case
          (compactor-tick chat (and (compactor-available-p agent)
                                    (null (agent-compact-function agent))
                                    (agent-client agent))
                          :model-function (agent-compact-function agent)
                          :tools (json-array (common-lisp-tool))
                          :system-prompt (agent-system-prompt agent))
        ;; Compaction is background work: it must never break a turn.
        (error () nil)))))

(defun await-summaries (agent id)
  "Wait until every message before ID is summarized, so the view shows the
whole chat before message ID. The steering line stays live and shows a
spinner meanwhile. The wait ends early, with a note in the transcript, when
the compactor cannot make progress (no usable model, or every request
failed), after *SUMMARY-WAIT-LIMIT* seconds, or when the user presses Ctrl-C;
the turn then goes ahead with the view up to the first missing summary.
Returns true when the view is complete."
  (let ((chat *chat*))
    (when (summarized-before-p chat id)
      (return-from await-summaries t))
    (record-entry :status "Summarizing earlier messages before sending… (Ctrl-C sends now)")
    (let ((deadline (+ (get-internal-real-time)
                       (* *summary-wait-limit* internal-time-units-per-second)))
          (frames "⠋⠙⠹⠸⠼⠴⠦⠧⠇⠏")
          (frame 0)
          (outcome nil))
      (handler-case
          (loop
            ;; Ctrl-C is held back while the chat changes and the steering
            ;; line is drawn, so it arrives during the sleep.
            (sb-sys:without-interrupts
              (tick-compactor agent chat)
              (setf outcome
                    (cond ((summarized-before-p chat id) :done)
                          ((or (not (compactor-available-p agent))
                               (compactor-idle-p chat))
                           "Memory summaries cannot be built now (see /memory).")
                          ((> (get-internal-real-time) deadline)
                           "Memory summaries are taking too long.")))
              (unless outcome
                (let ((*busy-text* (string (char frames (mod (incf frame) (length frames))))))
                  (when *input-poll* (funcall *input-poll*)))))
            (when outcome (return))
            (sleep 0.05))
        (sb-sys:interactive-interrupt ()
          (setf outcome "Interrupted.")))
      (unless (eq outcome :done)
        (record-entry :status
                      (format nil "~A Sending with the view up to message ~D; ~
                                   ~D earlier message~:P left out."
                              outcome (chat-leaf-frontier chat)
                              (- id (chat-leaf-frontier chat)))
                      :ok-p nil))
      (eq outcome :done))))

;;; The turn

(defun import-workspace-notes (agent)
  "Log the workspace's AGENTS.md as a note when it is new or was edited."
  (handler-case
      (let ((ids (import-agents-md *chat* (agent-workspace agent))))
        (when ids
          (record-entry :status
                        (format nil "Read ~A into memory."
                                (namestring (merge-pathnames
                                              "AGENTS.md"
                                              (project-root (agent-workspace agent)))))
                        :id (first ids))))
    (error (condition)
      (record-entry :status (format nil "Could not import AGENTS.md: ~A" condition)
                    :ok-p nil))))

(defun turn-state (agent)
  "The turn's date, workspace, and model, as the head of the user's message.
It changes every turn, so it comes after the view, never in the system prompt."
  (format nil "Date: ~A~%Workspace: ~A~%Model: ~A"
          (iso-8601 (get-universal-time))
          (namestring (agent-workspace agent))
          (client-model (agent-client agent))))

(defun turn-history (agent chat id prompt)
  "The start of the history for a turn on message ID: the view of every
message before it, then the turn state and PROMPT."
  (list (json-object "role" "user" "content" (render-view chat :end id))
        (json-object "role" "user"
                     "content" (format nil "~A~%~%~A" (turn-state agent) prompt))))

(defun run (prompt &key (agent (or *agent* (make-agent))) (print-response t))
  "Run one user PROMPT through AGENT until the model stops calling Lisp.
The prompt and everything the model says or runs are logged in *CHAT*; the
model sees the chat through its view. Prompts typed while the model works are
delivered before its next request. With PRINT-RESPONSE, tool calls and replies
are printed as they arrive; otherwise they are only recorded. Returns the
final reply."
  ;; Nested runs (including tests invoked by a tool) must restore the caller.
  (let* ((nested (and *agent* (not (eq agent *agent*))))
         ;; A run of another agent inside this one, such as tests a tool call
         ;; starts, keeps its entries and its messages to itself.
         (*session-entries* (if nested (make-entry-vector) *session-entries*))
         (*chat* (if (or nested (null *chat*)) (make-chat) *chat*))
         (*show-entries* print-response)
         (*agent* agent)
         (*steering* '()))
    (import-workspace-notes agent)
    ;; The prompt line already shows what the user typed.
    (let ((id (entry-id (log-entry :user prompt :show nil))))
      (await-summaries agent id)
      (setf (agent-history agent) (turn-history agent *chat* id prompt)))
    (loop
      (inject-steering agent)
      (let* ((response (invoke-model agent))
             (message (assistant-message response))
             (tool-calls (get-key message "tool_calls"))
             (content (message-content message)))
        (setf (agent-history agent)
              (append (agent-history agent) (list message)))
        (when (plusp (length content))
          (log-entry :maxwell content))
        (if (and (vectorp tool-calls) (plusp (length tool-calls)))
            (loop for call across tool-calls
                  do (setf (agent-history agent)
                           (append (agent-history agent)
                                   (list (execute-tool-call agent call))))
                     (tick-compactor agent))
            ;; A prompt typed while the model was answering continues the turn.
            (unless (steering-pending-p)
              (finish-pending-image)
              (return content)))))))
