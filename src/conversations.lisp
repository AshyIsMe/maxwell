(in-package #:maxwell)

;;; A subagent is an ordinary conversation with work in flight. Context is
;;; private; the Lisp runtime and filesystem are NOT isolated.
(defun find-conversation (name-or-id &optional (errorp t))
  (or (find name-or-id *conversations* :key #'conversation-name :test #'equal)
      (find name-or-id *conversations* :key #'conversation-id :test #'equal)
      (when errorp (error "No conversation ~S." name-or-id))))

(defun call-in-conversation (conversation function)
  (let ((*conversation* conversation)
        (*chat* (conversation-chat conversation))
        (*agent* (conversation-agent conversation))
        (*session-entries* (conversation-entries conversation)))
    (funcall function)))

(defun conversation-ui-wrapper (conversation)
  (lambda (function)
    (lambda () (call-in-conversation conversation function))))

(defun ensure-conversations (&optional (agent *agent*))
  ;; Upgrade the old singleton in place, preserving message IDs.
  (unless *conversations*
    (let ((main (%make-conversation :id (incf *next-conversation-id*)
                                   :name "main" :agent agent :chat *chat*
                                   :entries *session-entries*)))
      (setf *conversations* (list main) *selected-conversation* main)))
  (unless *selected-conversation* (setf *selected-conversation* (first *conversations*)))
  (select-conversation (conversation-name *selected-conversation*) :draw nil))

(defun create-conversation (name &key (from (or *conversation* *selected-conversation*)) snapshot workspace)
  "Create a fresh conversation, or a fixed context snapshot. No work starts."
  (bt:with-recursive-lock-held (*conversation-lock*)
    (unless (and (stringp name) (plusp (length name))
                 (notany (lambda (c) (find c '(#\Space #\Tab #\Newline))) name))
      (error "Conversation names must be nonempty, without whitespace."))
    (when (find-conversation name nil) (error "Conversation ~S already exists." name))
    (let* ((source (if from (conversation-agent from) *agent*))
           (chat (if snapshot
                     (with-chat-lock
                       (chat-from-export-form (copy-tree (chat-export-form (conversation-chat from)))))
                     (make-chat)))
           (client (copy-client (agent-client source)))
           (agent (make-agent :client client
                              :workspace (or workspace (agent-workspace source))
                              :system-prompt (agent-system-prompt source)
                              :model-function (agent-model-function source)
                              :compact-function (agent-compact-function source)))
           (conversation (%make-conversation :id (incf *next-conversation-id*)
                          :name name :parent (and from (conversation-id from))
                          :agent agent :chat chat)))
      (setf (client-extra-headers client) (copy-tree (client-extra-headers client)))
      (setf (client-request-fields client) (copy-tree-json (client-request-fields client)))
      (push conversation *conversations*)
      conversation)))

(defun copy-tree-json (value)
  (cond ((hash-table-p value)
         (let ((copy (make-hash-table :test 'equal)))
           (maphash (lambda (k v) (setf (gethash k copy) (copy-tree-json v))) value) copy))
        ((stringp value) (copy-seq value))
        ((vectorp value) (map 'vector #'copy-tree-json value))
        ((consp value) (mapcar #'copy-tree-json value))
        (t value)))

(defun select-conversation (name &key (draw t))
  "Select a transcript, independently of which conversations are running."
  (let ((conversation (find-conversation name)))
    (when (and draw *selected-conversation*)
      (setf (conversation-draft *selected-conversation*) *prompt-draft*))
    (setf *selected-conversation* conversation *conversation* conversation
          *agent* (conversation-agent conversation) *chat* (conversation-chat conversation)
          *session-entries* (conversation-entries conversation)
          *prompt-draft* (conversation-draft conversation)
          *older-entries-function* (older-log-entries *chat* (chat-message-count *chat*)))
    (hide-reasoning-trace)
    (clear-row-cache)
    (when draw
      (status-bar-hide)
      (format t "~&Conversation ~A (~A), model ~A~%" (conversation-name conversation)
              (conversation-state conversation) (client-model (agent-client *agent*)))
      (show-recent-exchanges (recent-log-entries *chat*) :count 2))
    conversation))

(defun conversation-running-p (conversation)
  (let ((turn (conversation-turn conversation)))
    (and turn (not (turn-done turn)))))

(defun send-conversation (name text &key (from *conversation*) wake)
  "Leave an attributed durable note. WAKE starts work at the next boundary."
  (bt:with-recursive-lock-held (*conversation-lock*)
    (let* ((target (find-conversation name))
           (note (make-conversation-note :id (incf *next-note-id*)
                   :from (and from (conversation-id from)) :text text :wake wake)))
      (setf (conversation-inbox target) (append (conversation-inbox target) (list note)))
      (when wake (ui-post (lambda () (service-conversations))))
      (conversation-note-id note))))

(defun deliver-conversation-notes (conversation)
  ;; Append and mark delivery in the same critical section. Never erase inbox
  ;; records: sender, note ID and delivery message ID remain inspectable.
  (bt:with-recursive-lock-held (*conversation-lock*)
    (with-chat-lock
      (dolist (note (conversation-inbox conversation))
        (unless (conversation-note-delivered note)
          (setf (conversation-note-delivered note)
                (first (chat-append (conversation-chat conversation) :note
                  (format nil "[note ~D from conversation ~A] ~A"
                    (conversation-note-id note) (or (conversation-note-from note) "user")
                    (conversation-note-text note))
                  :workspace (agent-workspace (conversation-agent conversation))))))))))

(defun start-conversation (name prompt &key report-to)
  "Start a task without selecting it. REPORT-TO receives a non-waking result."
  (bt:with-recursive-lock-held (*conversation-lock*)
    (let ((conversation (find-conversation name)))
      (when (conversation-running-p conversation) (error "~A is already running." name))
      (deliver-conversation-notes conversation)
      (call-in-conversation conversation
        (lambda ()
          (let ((*ui-context-wrapper* (conversation-ui-wrapper conversation)))
            (setf (conversation-report-to conversation) report-to
                  (conversation-state conversation) :running
                  (conversation-turn conversation)
                  (start-turn prompt :agent *agent* :print-response t
                    :bindings (list (cons '*reasoning-delta-function* nil)))))))
      conversation)))

(defun stop-conversation (name)
  (let ((conversation (find-conversation name)))
    (when (conversation-running-p conversation)
      (cancel-turn (conversation-turn conversation))
      (worker-join (turn-worker (conversation-turn conversation))))
    (setf (conversation-state conversation) :stopped)
    conversation))

(defun service-conversations ()
  "Reap completed work and start explicit wake requests. UI-thread only."
  (bt:with-recursive-lock-held (*conversation-lock*)
    (dolist (conversation *conversations*)
      (let ((turn (conversation-turn conversation)))
        (when (and turn (turn-done turn) (eq (conversation-state conversation) :running))
          (setf (conversation-state conversation)
                (cond ((turn-cancelled turn) :stopped)
                      ((turn-condition turn) :failed) (t :idle)))
          (when (conversation-report-to conversation)
            (send-conversation (conversation-report-to conversation)
              (format nil "Result from ~A (conversation ~D, ~A; inspect its transcript):~%~A"
                      (conversation-name conversation) (conversation-id conversation)
                      (conversation-state conversation)
                      (or (turn-condition turn) (turn-result turn) "Stopped without a result."))
              :from conversation)
            (setf (conversation-report-to conversation) nil))))
      (when (and (not (conversation-running-p conversation))
                 (some (lambda (n) (and (conversation-note-wake n)
                                        (not (conversation-note-delivered n))))
                       (conversation-inbox conversation)))
        (start-conversation (conversation-name conversation)
                            "Act on the pending messages delivered in your context.")))))

(defun conversation-command (argument)
  (let* ((words (remove "" (uiop:split-string argument :separator '(#\Space #\Tab)) :test #'equal))
         (verb (first words)) (name (second words))
         (text (format nil "~{~A~^ ~}" (cddr words))))
    (cond
      ((null verb)
       (service-conversations)
       (dolist (c (reverse *conversations*))
         (status "~A ~D ~A ~A — ~A (~D pending)"
                 (if (eq c *selected-conversation*) "*" " ")
                 (conversation-id c) (conversation-name c) (conversation-state c)
                 (client-model (agent-client (conversation-agent c)))
                 (count-if-not #'conversation-note-delivered (conversation-inbox c)))))
      ((member verb '("new" "fork") :test #'equal)
       (unless name (error "Usage: /chat ~A NAME" verb))
       (create-conversation name :snapshot (equal verb "fork"))
       (select-conversation name))
      ((equal verb "stop") (stop-conversation (or name (conversation-name *conversation*))))
      ((member verb '("note" "ask") :test #'equal)
       (unless (and name (plusp (length text))) (error "Usage: /chat ~A NAME TEXT" verb))
       (send-conversation name text :wake (equal verb "ask")))
      ((equal verb "run")
       (unless (and name (plusp (length text))) (error "Usage: /chat run NAME TASK"))
       (start-conversation name text :report-to (conversation-id *conversation*)))
      (t (select-conversation verb)))))

(defun conversation-idle ()
  (drain-ui-queue)
  (service-conversations))

(defun reset-conversation-runtime ()
  ;; Never replay an interrupted task or tool after process restart.
  (dolist (c *conversations*)
    (when (eq (conversation-state c) :running) (setf (conversation-state c) :interrupted))
    (setf (conversation-turn c) nil (conversation-steering c) nil
          (conversation-entries c) (make-entry-vector))
    (compactor-reset (conversation-chat c)))
  (setf *conversation* nil))
