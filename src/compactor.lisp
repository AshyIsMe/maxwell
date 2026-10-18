(in-package #:maxwell)

;;; Background compaction: async curl jobs that build OptChat summary nodes.
;;;
;;; COMPACTOR-TICK is called often (from the input poll, the idle prompt, and
;;; between tool calls). Each call reaps finished requests and starts queued
;;; nodes, up to *COMPACTOR-MAX-JOBS* at once. A request is a call like a
;;; turn, with the turns' exact system prompt and tools so it reads them from
;;; the same prompt-cache prefix, then the compaction view and the task.
;;; There are no threads: a job is a curl child process that is polled.

(defparameter *compactor-max-jobs* 8
  "The most compaction requests in flight at once.")

(defparameter *compaction-attempts* 5
  "Requests per node, counting the retries asking for a shorter line.")

(defparameter +ruler+ (make-string +node-limit+ :initial-element #\-)
  "Shows the model how long 512 bytes is, since models cannot count bytes.")

(defvar *compact-model-warned* nil
  "True once the cost warning about compacting with the main model was given.")

(defstruct compaction-job
  "One node being built. HANDLE is a PROVIDER-JOB, or, with a fake model
function, the response itself or a function that returns it once ready."
  key            ; (level . index)
  messages       ; the conversation so far, system prompt first
  tools
  client
  model-function
  (attempts 0)   ; requests made so far
  best           ; the shortest line received so far
  handle)

(defun compaction-client (client)
  "CLIENT with its compact model selected, or CLIENT itself when it has none."
  (let ((model (client-compact-model client)))
    (if (and model (string/= model (client-model client)))
        (let ((copy (copy-client client)))
          (setf (client-model copy) model)
          copy)
        client)))

(defun compact-model-warning (client)
  "Once per process, the warning to show when CLIENT compacts with its main
model; NIL otherwise. The compactor never prints, so the caller shows it."
  (when (and client (null (client-compact-model client)) (not *compact-model-warned*))
    (setf *compact-model-warned* t)
    (format nil "Memory summaries use the main model ~A, which may cost more than ~
                 a cheap one. Set --compact-model or MAXWELL_COMPACT_MODEL."
            (client-model client))))

;;; Requests

(defun compaction-context (chat level index)
  "The compaction view cut off at NODE(LEVEL, INDEX): built lines before its
message, or for a merge up to its last message, stopping at the first unbuilt."
  (render-lines chat (built-view-lines chat (chat-compaction-view chat)
                                       (if (zerop level)
                                           index
                                           (nth-value 1 (node-range level index))))))

(defun compaction-task (chat level index)
  "The task text for NODE(LEVEL, INDEX), worded as in the OptMem design."
  (if (zerop level)
      (let ((message (logged-message chat index)))
        (format nil "Compaction: compress message ~D into one line of at most 512 bytes
(about 70 words), the length of this ruler:
~A
<input>
~A~A: ~A
</input>"
                index +ruler+
                (project-tag (and (message-project message) (list (message-project message))))
                (kind-label (message-kind message)) (message-text message)))
      (multiple-value-bind (start end) (node-range level index)
        (let ((left (* 2 index)) (right (1+ (* 2 index))))
          (format nil "Compaction: merge lines ~A and ~A, adjacent, into one line of at most
512 bytes (about 70 words), the length of this ruler:
~A
<chat> may hold their messages, ~D to ~D, in more detail: take details
of them from there too.
<input>
~A
~A
</input>"
                  (node-name (1- level) left) (node-name (1- level) right) +ruler+
                  start (1- (min end (chat-message-count chat)))
                  (line-text chat (1- level) left) (line-text chat (1- level) right))))))

(defun compaction-messages (chat level index system-prompt)
  (list (json-object "role" "system" "content" system-prompt)
        (json-object "role" "user" "content" (compaction-context chat level index))
        (json-object "role" "user" "content" (compaction-task chat level index))))

(defun octet-prefix (string limit)
  "The longest prefix of STRING whose UTF-8 encoding fits in LIMIT bytes."
  (let ((octets (sb-ext:string-to-octets string :external-format :utf-8)))
    (if (<= (length octets) limit)
        string
        (let ((cut limit))
          ;; Never split a character: back up over continuation bytes.
          (loop while (and (plusp cut) (= (logand (aref octets cut) #xC0) #x80))
                do (decf cut))
          (sb-ext:octets-to-string octets :external-format :utf-8 :end cut)))))

(defun too-long-message (line)
  (format nil "Too long: your line is ~D bytes, over the 512-byte limit. Write
the whole line again for the same <input>, cutting just enough of the
least valuable items to fit before this cut:
~A| ← LIMIT"
          (octet-length line) (octet-prefix line +node-limit+)))

(defun strip-line-head (chat level index text)
  "TEXT without an id+n| head or project tag the model copied from its input."
  (let* ((text (string-trim '(#\Space #\Tab #\Newline #\Return) text))
         (bar (position #\| text)))
    (when (and bar (plusp bar)
               (every (lambda (character) (or (digit-char-p character) (char= character #\+)))
                      (subseq text 0 bar)))
      (setf text (string-left-trim " " (subseq text (1+ bar)))))
    (let ((tag (project-tag (node-projects-for chat level index))))
      (when (and (plusp (length tag)) (>= (length text) (length tag))
                 (string= tag text :end2 (length tag)))
        (setf text (subseq text (length tag)))))
    text))

(defun response-assistant-message (response)
  (let* ((choices (and (hash-table-p response) (get-key response "choices")))
         (choice (and (vectorp choices) (plusp (length choices)) (aref choices 0)))
         (message (and (hash-table-p choice) (get-key choice "message"))))
    (unless (hash-table-p message)
      (error 'provider-error :message "Compaction response has no assistant message."))
    message))

;;; Jobs

(defun send-compaction-request (job)
  "Send JOB's conversation, counting the attempt."
  (incf (compaction-job-attempts job))
  (setf (compaction-job-handle job)
        (if (compaction-job-model-function job)
            (funcall (compaction-job-model-function job)
                     (compaction-job-messages job) (compaction-job-tools job))
            (start-provider-request (compaction-job-client job)
                                    (compaction-job-messages job)
                                    (compaction-job-tools job))))
  job)

(defun compaction-reply (job)
  "JOB's response once it has arrived, else NIL. Signals when the call failed."
  (let ((handle (compaction-job-handle job)))
    (cond
      ((provider-job-p handle)
       (and (provider-request-finished-p handle)
            (progn (setf (compaction-job-handle job) nil)
                   (finish-provider-request handle))))
      ((functionp handle) (funcall handle))
      ((null handle) (error 'provider-error :message "Compaction request returned nothing."))
      (t handle))))

(defun abandon-job (job)
  (let ((handle (compaction-job-handle job)))
    (setf (compaction-job-handle job) nil)
    (when (provider-job-p handle)
      (cancel-provider-request handle))))

(defun shorter-line (a b)
  (cond ((null a) b)
        ((null b) a)
        ((< (octet-length b) (octet-length a)) b)
        (t a)))

(defun finish-node (chat key line)
  (remhash key (chat-jobs chat))
  (install-node chat (car key) (cdr key) line)
  (when (zerop (car key)) (release-leaves chat)))

(defun fail-job (chat job)
  "Put JOB's node back to be tried again after the next message."
  (abandon-job job)
  (let ((key (compaction-job-key job)))
    (remhash key (chat-jobs chat))
    (unless (member key (chat-retry chat) :test #'equal)
      (setf (chat-retry chat) (append (chat-retry chat) (list key))))))

(defun advance-job (chat job)
  "Handle JOB's reply if it has one: keep the line, ask for a shorter one, or
requeue the node when the call failed. Replies that are already there, as a
fake model's are, are handled in turn."
  (handler-case
      (loop
        (let ((response (compaction-reply job)))
          (unless response (return))
          (let* ((key (compaction-job-key job))
                 (message (response-assistant-message response))
                 (content (get-key message "content"))
                 (line (strip-line-head chat (car key) (cdr key)
                                        (if (stringp content) content ""))))
            (when (zerop (length line))
              (error 'provider-error :message "Compaction returned an empty line."))
            (setf (compaction-job-best job) (shorter-line (compaction-job-best job) line))
            (cond
              ((and (> (octet-length line) +node-limit+)
                    (< (compaction-job-attempts job) *compaction-attempts*))
               ;; Ask again in the same conversation, showing where the cut is.
               (setf (compaction-job-messages job)
                     (append (compaction-job-messages job)
                             (list message
                                   (json-object "role" "user"
                                                "content" (too-long-message line)))))
               (send-compaction-request job))
              (t
               ;; A few bytes over is fine: the view measures real sizes.
               (finish-node chat key (compaction-job-best job))
               (return))))))
    (error ()
      (fail-job chat job))))

(defun reap-compaction-jobs (chat)
  "Advance every in-flight job of CHAT whose reply has arrived."
  (let ((jobs '()))
    (maphash (lambda (key job) (declare (ignore key)) (push job jobs)) (chat-jobs chat))
    (dolist (job jobs)
      (advance-job chat job))))

(defun next-ready-node (chat)
  "Take the next node to build from CHAT's queue, leaves first: turns wait
for leaves, never for merges."
  (let* ((ready (chat-ready chat))
         (key (or (find 0 ready :key #'car) (first ready))))
    (when key
      (setf (chat-ready chat) (remove key ready :test #'equal :count 1)))
    key))

(defun start-compaction-job (chat key client model-function tools system-prompt)
  (let ((job (make-compaction-job
               :key key
               :messages (compaction-messages chat (car key) (cdr key) system-prompt)
               :tools tools
               :client client
               :model-function model-function)))
    (setf (gethash key (chat-jobs chat)) job)
    (handler-case (send-compaction-request job)
      (error () (fail-job chat job)))
    job))

(defun compactor-tick (chat client &key model-function
                                        (tools nil tools-p)
                                        (system-prompt *base-system-prompt*)
                                        (max-jobs *compactor-max-jobs*))
  "Reap CHAT's finished compaction requests and start queued nodes, keeping at
most MAX-JOBS in flight. Requests go to CLIENT's compact model, or, when
MODEL-FUNCTION is given, to it: it is called like an agent's model function,
(MESSAGES TOOLS), and returns the response, or a function that returns NIL
until the response is ready. TOOLS and SYSTEM-PROMPT must be the turns' own,
so compactions share their prompt-cache prefix. Never blocks on a real
provider; a fake MODEL-FUNCTION may answer at once. Returns the number of
jobs in flight."
  (reap-compaction-jobs chat)
  (when (or client model-function)
    (let ((tools (if tools-p tools (json-array (common-lisp-tool))))
          (client (and client (compaction-client client))))
      (loop
        (unless (< (hash-table-count (chat-jobs chat)) max-jobs) (return))
        (let ((key (next-ready-node chat)))
          (unless key (return))
          (unless (node-built-p chat (car key) (cdr key))
            (let ((job (start-compaction-job chat key client model-function
                                             tools system-prompt)))
              ;; A fake model may answer at once; reap it right away.
              (when (eq job (gethash key (chat-jobs chat)))
                (advance-job chat job))))))))
  (hash-table-count (chat-jobs chat)))

(defun compactor-idle-p (chat)
  "True when CHAT has no compaction in flight or ready to start."
  (and (zerop (hash-table-count (chat-jobs chat))) (null (chat-ready chat))))

(defun compactor-drain (chat timeout)
  "Before a checkpoint: let in-flight jobs finish for up to TIMEOUT seconds
(starting no new ones), then cancel the rest and put their nodes back at the
front of the queue. Afterwards CHAT refers to no process and can be saved.
Returns the number of jobs cancelled."
  (let ((deadline (+ (get-internal-real-time)
                     (round (* timeout internal-time-units-per-second)))))
    (loop
      (reap-compaction-jobs chat)
      (when (or (zerop (hash-table-count (chat-jobs chat)))
                (>= (get-internal-real-time) deadline))
        (return))
      (sleep 0.02)))
  (let ((keys '()))
    (maphash (lambda (key job) (abandon-job job) (push key keys)) (chat-jobs chat))
    (clrhash (chat-jobs chat))
    (setf (chat-ready chat)
          (append keys (chat-ready chat)))
    (length keys)))

(defun compactor-reset (chat)
  "After an image resumes: forget jobs saved by mistake, whose processes are
gone, and queue their nodes again. Never touches any process."
  (let ((keys '()))
    (maphash (lambda (key job) (declare (ignore job)) (push key keys)) (chat-jobs chat))
    (setf (chat-jobs chat) (make-hash-table :test 'equal)
          (chat-ready chat) (append keys (chat-ready chat)))
    (length keys)))
