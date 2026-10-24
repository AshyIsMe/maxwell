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
    ;; A provider that shows its thinking names it differently on each wire.
    (normalize-message-reasoning message)))

(defun invoke-model (agent)
  (ui-post-activity :thinking)
  ;; The live reasoning trace belongs to this request alone.
  (ui-call (lambda () (when (conversation-visible-p) (hide-reasoning-trace))))
  (unwind-protect
       (if (agent-model-function agent)
           (funcall (agent-model-function agent) (wire-messages agent)
                    (json-array (common-lisp-tool)))
           (call-provider (agent-client agent) (wire-messages agent)
                          (json-array (common-lisp-tool))))
    ;; The reply arrived, or the request failed: take the trace away, since the
    ;; model is done thinking.
    (ui-call (lambda () (when (conversation-visible-p) (hide-reasoning-trace))))
    (ui-post-activity :responding)))

;;; Logging

(defun log-entry (kind text &key (ok-p t) (show *show-entries*) (agent *agent*))
  "Log TEXT in *CHAT* as a KIND message from AGENT's workspace, and record it as
a transcript entry carrying its id. Returns the entry. A turn's worker may call
this, so the append takes the chat lock and the entry, which the UI thread
owns, is recorded through RECORD-ENTRY."
  (let ((ids (with-chat-lock
               (chat-append *chat* kind text :workspace (agent-workspace agent)))))
    (record-entry (if (eq kind :maxwell) :assistant kind) text
                  :ok-p ok-p :id (first ids) :show show)))

(defun tool-result-message (call result)
  ;; The model sees exactly what the log keeps: a huge result (say, a printed
  ;; structure of many megabytes) is clipped to its head and tail rather than
  ;; sent whole, which could exceed the provider's context limit.
  (json-object "role" "tool"
               "tool_call_id" (get-key call "id")
               "content" (clip-tool-output result)))

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

(defun tool-call-name (call)
  "The name of the function CALL invokes, for the status bar."
  (get-key (get-key call "function") "name"))

(defun execute-tool-call (agent call)
  "Run CALL and return its tool message. The call and its result are logged
and recorded as they happen, and both the log and the message sent back to
the model clip a very long result to its head and tail."
  (ui-post-activity :tooling (tool-call-name call))
  (multiple-value-prog1
      (progn
        (log-entry :tool (tool-call-text call) :agent agent)
        (multiple-value-bind (result ok-p) (evaluate-tool-call agent call)
          (log-entry :echo result :ok-p ok-p :agent agent)
          (tool-result-message call result)))
    (ui-post-activity :thinking)))

(defun message-content (message)
  (let ((content (get-key message "content")))
    (if (stringp content) content "")))

(defun finish-pending-image ()
  "Save the image the turn asked for. Only the UI thread can: the save forks
and ends the process, and it may not happen beside worker threads. On a worker
this does nothing, and the caller (RUN on the UI thread, or the interactive
turn once its worker has ended) carries the request out later."
  (when (and *pending-image* (ui-thread-p)
             (not (and (boundp '*conversations*)
                       (some #'conversation-running-p *conversations*))))
    (let ((path *pending-image*))
      (setf *pending-image* nil)
      ;; The snapshot ends the process, so the global chat is saved first.
      (when *checkpoint-path* (checkpoint))
      (save-image path))))

(defun pending-image-pending-p ()
  (not (null *pending-image*)))

;;; Turns on their own thread. The structure comes first, because the
;;; summary wait and RUN ask about the turn in flight.

(defstruct (turn (:constructor %make-turn))
  "One running turn: its worker, whether it was cancelled, and its outcome."
  prompt
  agent
  worker
  (cancelled nil)
  ;; Set while the turn waits for memory summaries, when Ctrl-C ends the wait
  ;; rather than the turn.
  (summary-waiting nil)
  (done nil)
  (result nil)
  (condition nil))

(defvar *current-turn* nil
  "The turn the UI thread is waiting on, so that Ctrl-C can cancel it.")

(defvar *in-turn* nil
  "True inside a turn's worker thread. A nested run -- a test suite a tool
call starts -- then runs in place rather than starting a thread of its own,
and keeps its transcript and its chat to itself.")

;;; Steering

(defvar *steering* '()
  "Prompts typed while a turn is running, oldest first. They reach the model
before its next request, so the user can redirect work already in progress.
The UI thread queues them and the turn's worker takes them, so the variable is
guarded by *STEERING-LOCK*.")

(defvar *steering-lock* (bt:make-lock "maxwell steering"))

(defun queue-steering (text)
  "Record TEXT as a steering prompt for the model's next request."
  (when (plusp (length (string-trim '(#\Space #\Tab) text)))
    (bt:with-lock-held (*steering-lock*)
      (if *conversation*
          (setf (conversation-steering *conversation*)
                (append (conversation-steering *conversation*) (list text)))
          (setf *steering* (append *steering* (list text)))))
    ;; The steering line already echoed it.
    (record-entry :steer text :show nil))
  text)

(defun steering-pending-p ()
  "True when a steering prompt is waiting to be delivered."
  (bt:with-lock-held (*steering-lock*)
    (not (null (if *conversation* (conversation-steering *conversation*) *steering*)))))

(defun take-steering ()
  "Remove and return the queued steering prompts, oldest first."
  (bt:with-lock-held (*steering-lock*)
    (if *conversation*
        (prog1 (conversation-steering *conversation*)
          (setf (conversation-steering *conversation*) nil))
        (prog1 *steering* (setf *steering* '())))))

(defun inject-steering (agent)
  "Log queued steering prompts as :USER messages and append them to AGENT's
history. The transcript and the chat belong to the UI thread, so the entry's id
is filled in there."
  (let ((pending (take-steering)))
    (when pending
      (let ((ids (with-chat-lock
                   (loop for text in pending
                         collect (first (chat-append *chat* :user text
                                                     :workspace (agent-workspace agent)))))))
        (ui-call
          (lambda ()
            (loop for text in pending
                  for id in ids
                  do (let ((entry (find-if (lambda (entry)
                                             (and (eq (entry-kind entry) :steer)
                                                  (null (entry-id entry))
                                                  (string= (entry-text entry) text)))
                                           *session-entries* :from-end t)))
                       (when entry (setf (entry-id entry) id)))))))
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

(defun summary-waiting-p ()
  "True while the current turn waits for memory summaries. Ctrl-C ends that
wait rather than the turn, so the UI thread leaves the interrupt to the turn's
own thread then. The flag lives on the turn, not in a dynamic binding: the UI
thread asks about a turn that runs on another thread."
  (and *current-turn* (turn-summary-waiting *current-turn*)))

(defparameter *summary-wait-limit* 600
  "The most seconds a turn waits for earlier messages to be summarized.")

(defun compactor-available-p (agent)
  "True when AGENT can make compaction requests: through its compact function,
or through its client when it has no fake model."
  (or (agent-compact-function agent) (null (agent-model-function agent))))

(defun tick-compactor (&optional (agent *agent*) (chat *chat*))
  "Advance CHAT's background compaction for AGENT without blocking. Requests
reuse the turns' system prompt and tools, so they share their cache prefix.
The UI thread may call this while a turn's worker also does, so the tick takes
the chat lock."
  (when (and agent chat)
    (handler-case
        (with-chat-lock
          (compactor-tick chat (and (compactor-available-p agent)
                                    (null (agent-compact-function agent))
                                    (agent-client agent))
                          :model-function (agent-compact-function agent)
                          :tools (json-array (common-lisp-tool))
                          :system-prompt (agent-system-prompt agent)))
      ;; Compaction is background work: it must never break a turn.
      (error () nil))))

(defun set-busy-text (text)
  "Show TEXT, such as a spinner frame, before the steering prompt."
  (if (ui-thread-p)
      (when (conversation-visible-p) (setf *busy-text* text))
      (ui-post (lambda () (when (conversation-visible-p) (setf *busy-text* text))))))

(defun await-summaries (agent id)
  "Wait until every message before ID is summarized, so the view shows the
whole chat before message ID. The steering line stays live and shows a
spinner meanwhile. The wait ends early, with a note in the transcript, when
the compactor cannot make progress (no usable model, or every request
failed), after *SUMMARY-WAIT-LIMIT* seconds, or when the turn is cancelled;
the turn then goes ahead with the view up to the first missing summary.
Returns true when the view is complete."
  (let ((chat *chat*))
    (when (with-chat-lock (summarized-before-p chat id))
      (return-from await-summaries t))
    (ui-post-activity :summarizing)
    (record-entry :status "Summarizing earlier messages before sending… (Ctrl-C sends now)")
    ;; While this wait runs, Ctrl-C ends the wait rather than the turn, so the
    ;; UI thread leaves the interrupt to this thread.
    (when *current-turn* (setf (turn-summary-waiting *current-turn*) t))
    (let ((deadline (+ (get-internal-real-time)
                       (* *summary-wait-limit* internal-time-units-per-second)))
          (frames "⠋⠙⠹⠸⠼⠴⠦⠧⠇⠏")
          (frame 0)
          (outcome nil))
      (unwind-protect
           (loop
             (tick-compactor agent chat)
             (setf outcome
                   (cond ((with-chat-lock (summarized-before-p chat id)) :done)
                         ((take-interrupt) "Interrupted.")
                         ((turn-cancelled-p) "Interrupted.")
                         ((or (not (compactor-available-p agent))
                              (with-chat-lock (compactor-idle-p chat)))
                          "Memory summaries cannot be built now (see /memory).")
                         ((> (get-internal-real-time) deadline)
                          "Memory summaries are taking too long.")))
             (unless outcome
               (set-busy-text (string (char frames (mod (incf frame) (length frames))))))
             (when outcome (return))
             (sleep 0.05))
        (when *current-turn* (setf (turn-summary-waiting *current-turn*) nil))
        (set-busy-text nil))
      (unless (eq outcome :done)
        ;; Last words outside the lock: RECORD-ENTRY runs on the UI thread.
        (let ((frontier (with-chat-lock (chat-leaf-frontier chat))))
          (record-entry :status
                        (format nil "~A Sending with the view up to message ~D; ~
                                     ~D earlier message~:P left out."
                                outcome frontier (- id frontier))
                        :ok-p nil)))
      (ui-post-activity :thinking)
      (eq outcome :done))))

;;; The turn

(defun import-workspace-notes (agent)
  "Log the workspace's AGENTS.md as a note when it is new or was edited. The
note goes into the chat under the chat lock; the transcript entry is recorded
outside it, because a worker records entries through the UI thread and the UI
thread must never be asked for something while the chat lock is held."
  (handler-case
      (let ((ids (with-chat-lock (import-agents-md *chat* (agent-workspace agent)))))
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
  (format nil "Date: ~A~%Workspace: ~A~%Model: ~A~@[~%Conversation: ~A~]"
          (iso-8601 (get-universal-time))
          (namestring (agent-workspace agent))
          (client-model (agent-client agent))
          (and *conversation* (format nil "~A (#~D)" (conversation-name *conversation*)
                                      (conversation-id *conversation*)))))

(defun turn-history (agent chat id prompt)
  "The start of the history for a turn on message ID: the view of every
message before it, then the turn state and PROMPT."
  (list (json-object "role" "user" "content" (render-view chat :end id))
        (json-object "role" "user"
                     "content" (format nil "~A~%~%~A" (turn-state agent) prompt))))

(defun %run-turn (prompt agent print-response)
  "The turn itself, on whatever thread calls it. RUN and START-TURN both end
up here; the work always belongs to a worker thread, except in a one-shot run
or a test, where there is no UI thread to keep free."
  ;; Nested runs (including tests invoked by a tool) must restore the caller.
  (let* ((nested (and *agent* (not (eq agent *agent*))))
         ;; A run of another agent inside this one, such as tests a tool call
         ;; starts, keeps its entries and its messages to itself.
         (*session-entries* (if nested (make-entry-vector) *session-entries*))
         (*chat* (if (or nested (null *chat*)) (make-chat) *chat*))
         (*show-entries* print-response)
         (*agent* agent))
    (import-workspace-notes agent)
    ;; The prompt line already shows what the user typed.
    (let ((id (entry-id (log-entry :user prompt :show nil))))
      (await-summaries agent id)
      (with-chat-lock
        (setf (agent-history agent) (turn-history agent *chat* id prompt))))
    (loop
      (inject-steering agent)
      (let* ((response (invoke-model agent))
             (message (assistant-message response))
             (tool-calls (get-key message "tool_calls"))
             (content (message-content message)))
        ;; What the model thought is recorded, but hidden until Ctrl-T: it was
        ;; on screen while the model thought, and it is not part of the view.
        (let ((reasoning (message-reasoning message)))
          (when reasoning (record-entry :thinking reasoning :show *show-entries*)))
        (setf (agent-history agent)
              (append (agent-history agent) (list message)))
        (when (plusp (length content))
          (log-entry :maxwell content))
        (if (and (vectorp tool-calls) (plusp (length tool-calls)))
            (loop for call across tool-calls
                  for index from 0
                  do (when (plusp index) (pump-ui))
                     (setf (agent-history agent)
                           (append (agent-history agent)
                                   (list (execute-tool-call agent call))))
                     ;; The compactor has a worker of its own at the prompt, so this
                     ;; only advances it where there is none: a one-shot run or a test.
                     (when (or *conversation* (not (compactor-worker-live-p))) (tick-compactor agent)))
            ;; A prompt typed while the model was answering continues the turn.
            (unless (steering-pending-p)
              (finish-pending-image)
              (return content)))))))

(defun run (prompt &key (agent (or *agent* (make-agent))) (print-response t))
  "Run one user PROMPT through AGENT until the model stops calling Lisp.
The prompt and everything the model says or runs are logged in *CHAT*; the
model sees the chat through its view. Prompts typed while the model works are
delivered before its next request. With PRINT-RESPONSE, tool calls and replies
are printed as they arrive; otherwise they are only recorded. Returns the
final reply.

On the UI thread a turn would freeze the prompt, so there it runs on a worker
and this call waits for it, servicing keys meanwhile; inside a turn already,
or when no thread owns the terminal, it runs in place. START-TURN is the
version that hands back the running turn instead of waiting."
  (if (and *ui-thread* (ui-thread-p) (not *in-turn*))
      (multiple-value-prog1
          (await-turn (start-turn prompt :agent agent :print-response print-response))
        ;; A requested image is saved now, after its turn's worker has ended.
        (finish-pending-image))
      (%run-turn prompt agent print-response)))

;;; Turns on their own thread

(defun turn-cancelled-p ()
  "True when the turn this thread belongs to was cancelled, as Ctrl-C does."
  (and *current-turn* (turn-cancelled *current-turn*)))

(defun turn-finished (turn result condition)
  (setf (turn-result turn) result
        (turn-condition turn) condition
        (turn-done turn) t)
  turn)

(defun turn-main (turn prompt agent print-response)
  (handler-case
      (turn-finished turn (%run-turn prompt agent print-response) nil)
    (error (condition)
      (turn-finished turn nil condition))
    ;; A cancelled turn unwinds through this, keeping what it logged so far.
    (sb-sys:interactive-interrupt ()
      (turn-finished turn nil nil))))

(defun start-turn (prompt &key (agent (or *agent* (make-agent)))
                          (print-response t) (bindings '()))
  "Start PROMPT on a worker thread and return its TURN at once. The worker
inherits the chat, the agent, and the transcript's settings: dynamic bindings
do not reach a new thread by themselves, so they are passed explicitly. Extra
BINDINGS, an alist of (SPECIAL . VALUE), reach the worker too."
  (let* ((turn (%make-turn :prompt prompt :agent agent))
         ;; The steering queue stays the one the UI thread queues into: the
         ;; worker takes from the same place, so a prompt typed during the
         ;; turn reaches the model. Anything left over is dropped.
         (bindings (append bindings
                           (list (cons '*conversation* *conversation*)
                                  (cons '*ui-context-wrapper* *ui-context-wrapper*)
                                  (cons '*chat* *chat*)
                                 (cons '*agent* agent)
                                 (cons '*session-entries* *session-entries*)
                                 (cons '*show-entries* print-response)
                                 (cons '*current-turn* turn)
                                 (cons '*in-turn* t)))))
    (take-steering)
    (setf (turn-worker turn)
          (start-worker "maxwell turn"
                        (lambda () (turn-main turn prompt agent print-response))
                        :bindings bindings))
    (when *conversation*
      (setf (conversation-turn *conversation*) turn
            (conversation-state *conversation*) :running))
    turn))

(defun cancel-turn (turn)
  "Abandon TURN: stop its worker, which unwinds through cleanup and keeps
whatever it already logged. Returns true when there was a turn to cancel."
  (when (and turn (not (turn-done turn)))
    (setf (turn-cancelled turn) t)
    (let ((thread (worker-thread (turn-worker turn))))
      (ignore-errors (bt:destroy-thread thread)))
    (setf (turn-done turn) t
          (turn-result turn) nil)
    t))

(defun await-turn (turn)
  "Wait for TURN on the UI thread, keeping the prompt live: keys, queued work,
and the compactor all run here while the turn's worker does the real work.
Ctrl-C cancels the turn there. Returns the reply; re-signals the error the
turn ended with, if any."
  (let ((*current-turn* turn)
        (origin *conversation*))
    (unwind-protect
         (progn
           (loop until (or (turn-done turn)
                                 (and origin
                                      (not (eq origin *selected-conversation*))))
                 do (when (and (not (summary-waiting-p)) (take-interrupt))
                      (record-entry :status "Interrupted." :ok-p nil :show t)
                      (cancel-turn turn))
                    (service-ui)
                    (sleep 0.01))
           (when (turn-condition turn) (error (turn-condition turn)))
           (turn-result turn))
      ;; A turn interrupted from outside still may run cleanup on its worker.
      (when (and (turn-done turn) (turn-worker turn) (worker-alive-p (turn-worker turn)))
        (ignore-errors (worker-join (turn-worker turn)))))))















