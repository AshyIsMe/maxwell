(in-package #:maxwell/tests)

;;; Turns over the endless chat, the global image's helpers, and the command line.

(defun content (message)
  (maxwell/json:get-key message "content"))

(defun recording-model (requests &key (reply (lambda (n) (format nil "reply ~D" n))))
  "A model that pushes each request's messages onto the first element of
REQUESTS and answers with REPLY of the request count."
  (lambda (messages tools)
    (declare (ignore tools))
    (push messages (first requests))
    (fake-response :content (funcall reply (length (first requests))))))

(defun view-prefix (view)
  "VIEW without its closing tag, which every later view must start with."
  (subseq view 0 (- (length view) (length "</chat>"))))

(defun test-turn-assembly ()
  (with-fresh-chat (chat)
    (let* ((requests (list '()))
           (agent (maxwell:make-agent :model-function (recording-model requests))))
      (dotimes (n 4)
        (maxwell:run (format nil "question ~D" n) :agent agent :print-response nil))
      (let ((turns (reverse (first requests))))
        ;; [system] [view] [turn state + message], and nothing carried over.
        (check (every (lambda (messages) (= 3 (length messages))) turns))
        ;; The system prompt is byte-identical in every turn.
        (check (every (lambda (messages)
                        (equalp (sb-ext:string-to-octets (content (first messages)))
                                (sb-ext:string-to-octets maxwell::*base-system-prompt*)))
                      turns))
        ;; The view grows only at its end.
        (loop for (earlier later) on turns
              while later
              do (check (eql 0 (search (view-prefix (content (second earlier)))
                                       (content (second later))))))
        (check (string= (format nil "<chat>~%</chat>") (content (second (first turns)))))
        (let ((view (content (second (second turns)))))
          (check (search "0+1|" view))
          (check (search "] user: question 0" view))
          (check (search "] maxwell: reply 1" view)))
        ;; The turn state leads the message, never the system prompt.
        (let ((message (content (third (first turns)))))
          (check (eql 0 (search "Date: " message)))
          (check (search (format nil "Workspace: ~A" (namestring (maxwell:agent-workspace agent)))
                         message))
          (check (search (format nil "~%~%question 0") message)))
        (check (not (search "Workspace: /" maxwell::*base-system-prompt*))))))
  ;; Across merge batches the view only grows at its end or shrinks in a batch.
  (with-fresh-chat (chat)
    (let* ((requests (list '()))
           (maxwell::*view-high-water* 1200)
           (maxwell::*view-low-water* 600)
           (agent (maxwell:make-agent
                    :model-function (recording-model
                                      requests
                                      :reply (lambda (n) (format nil "reply ~D ~A" n
                                                                 (long-text 200 #\r))))
                    :compact-function (lambda (messages tools)
                                        (declare (ignore messages tools))
                                        (fake-response :content "merged"))))
           (batches 0)
           (churned 0))
      (dotimes (n 30)
        (maxwell:run (format nil "question ~D" n) :agent agent :print-response nil)
        ;; As the idle prompt does between turns.
        (maxwell::tick-compactor agent chat))
      (loop for (earlier later) on (reverse (first requests))
            while later
            do (let ((a (content (second earlier)))
                     (b (content (second later))))
                 (cond ((eql 0 (search (view-prefix a) b)))
                       ((< (length b) (length a)) (incf batches))
                       (t (incf churned)))))
      (check (plusp batches))
      (check (zerop churned)))))

(defun test-turn-logging ()
  (with-fresh-chat (chat)
    (let* ((long-result (format nil "(make-string 40000 :initial-element #\\x)"))
           (responses (list (fake-response
                              :content "Working on it."
                              :tool-calls (maxwell/json:json-array (tool-call long-result)))
                            (fake-response :content "Done.")))
           (agent (maxwell:make-agent :model-function (lambda (messages tools)
                                                        (declare (ignore messages tools))
                                                        (pop responses))))
           (maxwell::*session-entries* (maxwell::make-entry-vector)))
      (maxwell:run "Make a long string." :agent agent :print-response nil)
      ;; Everything is logged as it happens, in order.
      (check (equal '(:user :maxwell :tool :echo :maxwell)
                    (map 'list #'maxwell::message-kind (maxwell::chat-messages chat))))
      ;; The in-turn history clips a huge result just as the log does,
      ;; so no single tool message can blow the provider's context limit.
      (let ((tool-message (find "tool" (maxwell:agent-history agent)
                                :key (lambda (message) (maxwell/json:get-key message "role"))
                                :test #'equal)))
        (check (<= (length (content tool-message)) 30000))
        (check (search "clipped" (content tool-message))))
      (check (<= (length (maxwell::message-text (maxwell::logged-message chat 3))) 30000))
      ;; Transcript entries carry their log ids.
      (check (equal '(0 1 2 3 4)
                    (map 'list #'maxwell::entry-id maxwell::*session-entries*)))
      (check (equal '(:user :assistant :tool :echo :assistant)
                    (map 'list #'maxwell::entry-kind maxwell::*session-entries*)))))
  ;; AGENTS.md is read at the start of a turn, before the message.
  (let ((base (scratch-directory "turn-agents")))
    (unwind-protect
         (with-fresh-chat (chat)
           (ensure-directories-exist (merge-pathnames ".git/" base))
           (with-open-file (stream (merge-pathnames "AGENTS.md" base) :direction :output)
             (write-string "Always answer in French." stream))
           (let* ((requests (list '()))
                  (agent (maxwell:make-agent :workspace base
                                             :model-function (recording-model requests))))
             (maxwell:run "Bonjour" :agent agent :print-response nil)
             (maxwell:run "Encore" :agent agent :print-response nil)
             (check (equal '(:note :user :maxwell :user :maxwell)
                           (map 'list #'maxwell::message-kind (maxwell::chat-messages chat))))
             (check (search "Always answer in French."
                            (content (second (first (last (first requests)))))))))
      (uiop:delete-directory-tree base :validate t :if-does-not-exist :ignore))))

(defun test-summary-wait ()
  ;; With no way to build summaries, the turn goes ahead and says so.
  (with-fresh-chat (chat)
    (maxwell::chat-append chat :user (long-text 600))
    (let* ((requests (list '()))
           (agent (maxwell:make-agent :model-function (recording-model requests)))
           (maxwell::*session-entries* (maxwell::make-entry-vector)))
      (check (string= "reply 1" (maxwell:run "next" :agent agent :print-response nil)))
      (let ((note (find-if (lambda (entry)
                             (and (eq (maxwell::entry-kind entry) :status)
                                  (not (maxwell::entry-ok-p entry))))
                           maxwell::*session-entries*)))
        (check (and note (search "cannot be built" (maxwell::entry-text note))))
        (check (and note (search "message 0; 1 earlier message left out" (maxwell::entry-text note)))))
      (check (string= (format nil "<chat>~%</chat>") (content (second (first (first requests))))))))
  ;; With a compactor, the turn waits for the summary and shows it.
  (with-fresh-chat (chat)
    (maxwell::chat-append chat :user (long-text 600))
    (let* ((requests (list '()))
           (agent (maxwell:make-agent
                    :model-function (recording-model requests)
                    :compact-function (lambda (messages tools)
                                        (declare (ignore messages tools))
                                        (fake-response :content "user: a long text")))))
      (maxwell:run "next" :agent agent :print-response nil)
      (check (search "0+1|user: a long text" (content (second (first (first requests)))))))))

(defun test-log-paging ()
  (with-fresh-chat (chat)
    (dotimes (n 10) (maxwell::chat-append chat :user (format nil "m~D" n)))
    (let ((older (maxwell::older-log-entries chat 6)))
      ;; Before the session's own entries, then back from a logged entry.
      (check (equal '(2 3 4 5) (map 'list #'maxwell::entry-id (funcall older nil 4))))
      (check (equal '(2 3 4 5)
                    (map 'list #'maxwell::entry-id
                         (funcall older (maxwell::make-entry :status "banner") 4))))
      (check (equal '(0 1 2)
                    (map 'list #'maxwell::entry-id
                         (funcall older (maxwell::log-message-entry
                                          (maxwell::logged-message chat 3))
                                  10))))
      (check (zerop (length (funcall older (maxwell::log-message-entry
                                             (maxwell::logged-message chat 0))
                                     10)))))
    (check (equal '("m8" "m9")
                  (map 'list #'maxwell::entry-text (maxwell::recent-log-entries chat 2))))))

(defun test-image-paths-and-lock ()
  (flet ((environment (&rest pairs)
           (lambda (name) (second (assoc name (loop for (k v) on pairs by #'cddr
                                                    collect (list k v))
                                         :test #'string=)))))
    (check (string= "/images/mine.image"
                    (namestring (maxwell::global-image-path
                                  (environment "MAXWELL_IMAGE" "/images/mine.image"
                                               "XDG_DATA_HOME" "/data")))))
    (check (string= "/data/maxwell/maxwell.image"
                    (namestring (maxwell::global-image-path
                                  (environment "XDG_DATA_HOME" "/data")))))
    (check (search ".local/share/maxwell/maxwell.image"
                   (namestring (maxwell::global-image-path (environment)))))
    (check (string= "/data/maxwell/maxwell.lock"
                    (namestring (maxwell::image-lock-path #p"/data/maxwell/maxwell.image"))))
    (check (string= "/data/maxwell/maxwell.image.tmp"
                    (maxwell::checkpoint-temporary-path #p"/data/maxwell/maxwell.image"))))
  (check (not (maxwell::running-global-image-p #p"/nonexistent/maxwell.image")))
  ;; The lock admits one holder; it is free again once released.
  (let* ((base (scratch-directory "lock"))
         (path (merge-pathnames "maxwell.lock" base)))
    (unwind-protect
         (let ((maxwell::*lock-fd* nil))
           (check (maxwell::acquire-chat-lock path))
           (let ((held maxwell::*lock-fd*)
                 (maxwell::*lock-fd* nil))
             (check (null (maxwell::acquire-chat-lock path)))
             (setf maxwell::*lock-fd* held)
             (maxwell::release-chat-lock))
           (check (maxwell::acquire-chat-lock path))
           (maxwell::release-chat-lock)
           (check (null maxwell::*lock-fd*)))
      (uiop:delete-directory-tree base :validate t :if-does-not-exist :ignore))))

(defun test-chat-transplant ()
  (let ((base (scratch-directory "transplant")))
    (unwind-protect
         (with-fresh-chat (chat)
           (ensure-directories-exist (merge-pathnames ".git/" base))
           (maxwell::chat-append chat :user "héllo" :workspace base :time 100)
           (maxwell::chat-append chat :maxwell (long-text 600) :workspace base)
           (maxwell::chat-append chat :tool "(+ 1 2)")
           (drain-synchronously chat (fake-line-model))
           (maxwell::chat-append chat :echo (long-text 700))
           ;; A job in flight is queued again in the copy, never carried over.
           (maxwell::compactor-tick chat nil :model-function (lambda (m tl)
                                                               (declare (ignore m tl))
                                                               (constantly nil))
                                    :tools #())
           (check (= 1 (hash-table-count (maxwell::chat-jobs chat))))
           (let* ((text (with-output-to-string (stream) (maxwell::write-chat chat stream)))
                  (copy (with-input-from-string (stream text) (maxwell::read-chat stream))))
             ;; Plain data only: no structures, no base-string #A syntax.
             (check (not (search "#S(" text)))
             (check (not (search "#A(" text)))
             (check (zerop (hash-table-count (maxwell::chat-jobs copy))))
             (check (equal '((0 . 3)) (maxwell::chat-ready copy)))
             (check (string= (maxwell::render-view chat) (maxwell::render-view copy)))
             (check (string= (maxwell::memory-report chat) (maxwell::memory-report copy)))
             (check (equal (maxwell::chat-projects chat) (maxwell::chat-projects copy)))
             (let ((zoomed (maxwell:zoom 0 1)))
               (check (search "user: héllo" zoomed))
               (let ((maxwell:*chat* copy))
                 (check (string= zoomed (maxwell:zoom 0 1)))
                 (check (string= (maxwell::iso-8601 100) (maxwell:date 0)))))
             ;; The copy keeps working like the original.
             (maxwell::chat-append copy :user "more")
             (check (= 5 (maxwell::chat-message-count copy)))))
      (uiop:delete-directory-tree base :validate t :if-does-not-exist :ignore)))
  ;; Reading never evaluates, and only an exported chat is accepted.
  (check (signals-error-p
           (lambda ()
             (with-input-from-string
                 (stream "(:maxwell-chat :version #.(setf maxwell/tests::*transplant-evaluated* 1))")
               (maxwell::read-chat stream)))))
  (check (not (boundp '*transplant-evaluated*)))
  (check (signals-error-p (lambda () (maxwell::chat-from-export-form '(:other)))))
  (check (signals-error-p (lambda () (maxwell::chat-from-export-form '(:maxwell-chat :version 99))))))

(defun test-startup-migration ()
  ;; An old image's linear conversation moves into the chat on first start.
  (let* ((maxwell:*chat* nil)
         (maxwell::*session-entries* (maxwell::make-entry-vector))
         (maxwell::*checkpoint-path* nil)
         (maxwell:*agent* (maxwell:make-agent)))
    (setf (maxwell:agent-history maxwell:*agent*)
          (list (maxwell/json:json-object "role" "user" "content" "Old question.")
                (maxwell/json:json-object "role" "assistant" "content" "Old answer.")))
    (let ((*standard-output* (make-broadcast-stream)))
      (maxwell::open-global-chat '(:ephemeral t) '()))
    (check (null (maxwell:agent-history maxwell:*agent*)))
    (check (null maxwell::*checkpoint-path*))
    (check (equal '("Old question." "Old answer.")
                  (map 'list #'maxwell::message-text (maxwell::chat-messages maxwell:*chat*))))))

(defun test-command-line ()
  (let ((options (maxwell::parse-options
                   '("-m" "main" "--compact-model" "cheap" "-H" "A: 1" "--fresh"
                     "--header" "B: 2" "--ephemeral" "say" "hello"))))
    (check (string= "main" (getf options :model)))
    (check (string= "cheap" (getf options :compact-model)))
    (check (equal '("A: 1" "B: 2") (getf options :headers)))
    (check (getf options :fresh))
    (check (getf options :ephemeral))
    (check (not (getf options :adopt-chat)))
    (check (string= "say hello" (getf options :prompt))))
  (check (getf (maxwell::parse-options '("--export-chat")) :export-chat))
  (check (signals-error-p (lambda () (maxwell::parse-options '("--bogus")))))
  (check (signals-error-p (lambda () (maxwell::parse-options '("--model")))))
  ;; --compact-model and MAXWELL_COMPACT_MODEL reach the client.
  (check (string= "cheap" (getf (maxwell::resolve-client-options
                                  :base-url "https://x.example/v1" :compact-model "cheap"
                                  :environment (constantly nil))
                                :compact-model)))
  (check (string= "env-cheap"
                  (getf (maxwell::resolve-client-options
                          :base-url "https://x.example/v1"
                          :environment (lambda (name)
                                         (and (string= name "MAXWELL_COMPACT_MODEL")
                                              "env-cheap")))
                        :compact-model)))
  ;; The thinking effort is remembered with the model, and reaches the client.
  (let* ((base (scratch-directory "session-effort"))
         (file (merge-pathnames "session.json" base))
         (client (maxwell:make-client :model "main" :base-url "https://x.example/v1"
                                      :effort "high"))
         (maxwell::*session-file* file))
    (unwind-protect
         (progn
           (check (string= "high"
                           (maxwell/json:get-key (maxwell::session-record-for-client client)
                                                 "effort")))
           (maxwell::remember-client client)
           (check (string= "high" (getf (maxwell::resolve-client-options
                                            :environment (constantly nil))
                                          :effort)))
           ;; Naming a provider starts a fresh configuration, so the effort of
           ;; the previous session does not follow it.
           (check (null (getf (maxwell::resolve-client-options
                                :base-url "https://y.example/v1"
                                :environment (constantly nil))
                              :effort))))
      (uiop:delete-directory-tree base :validate t :if-does-not-exist :ignore)))
  ;; Bad options make MAIN fail with status 1, and IMAGE-MAIN exits with it.
  ;; MAIN also picks AUTO colour from stdout, which may be a real terminal
  ;; here, so the colour is put back the way it was for the caller.
  (let ((color maxwell::*color*))
    (let ((*error-output* (make-broadcast-stream)))
      (check (= 1 (maxwell:main '("--color" "bogus"))))
      (check (= 1 (maxwell:main '("--no-such-option")))))
    (setf maxwell::*color* color)))

