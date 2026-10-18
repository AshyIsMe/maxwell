(in-package #:maxwell/tests)

;;; The OptChat store, its view, and the background compactor.

(defmacro with-fresh-chat ((chat) &body body)
  "Run BODY with maxwell:*chat* bound to a new, empty chat named CHAT."
  `(let* ((,chat (maxwell::make-chat))
          (maxwell:*chat* ,chat))
     ,@body))

(defun signals-error-p (thunk)
  (handler-case (progn (funcall thunk) nil)
    (error () t)))

(defun fake-line-model (&key (line-bytes 300) (calls (list 0)))
  "A synchronous compactor model that answers every task with a LINE-BYTES line."
  (lambda (messages tools)
    (declare (ignore messages tools))
    (incf (first calls))
    (fake-response :content (make-string line-bytes :initial-element #\x))))

(defun long-text (bytes &optional (character #\a))
  (make-string bytes :initial-element character))

(defun scratch-directory (name)
  (uiop:ensure-directory-pathname
    (merge-pathnames (format nil "maxwell-test-~A-~A/" name (maxwell::random-value 6))
                     (uiop:temporary-directory))))

;;; Taelin's rollback push, as in the gist: a newest-first list of
;;; (keep . state). A 0 absorbs the new state; a 1 carries the old one down.

(defun taelin-push (new states)
  (if (null states)
      (list (cons 0 new))
      (destructuring-bind ((keep . state) . older) states
        (if (zerop keep)
            (cons (cons 1 state) older)
            (cons (cons 0 new) (taelin-push state older))))))

(defun push-lines (states total)
  "STATES read as view lines (level . index), oldest first."
  (let ((starts (reverse (mapcar #'cdr states))))
    (loop for (start next) on starts
          for width = (- (or next total) start)
          collect (cons (1- (integer-length width)) (/ start width)))))

(defun test-view-matches-push ()
  (let ((states '())
        (view (maxwell::make-growable-vector))
        (mismatches 0)
        (stuck 0))
    (loop for tick from 0 to 20000
          for total = (1+ tick)
          do (setf states (taelin-push tick states))
             (vector-push-extend (cons 0 tick) view)
             (loop while (> (length view) (length states))
                   do (let ((position (maxwell::most-due-pair
                                        view total (constantly t))))
                        (unless position (incf stuck) (return))
                        (maxwell::merge-view-pair view position)))
             (unless (equal (coerce view 'list) (push-lines states total))
               (incf mismatches)))
    (check (zerop stuck))
    (check (zerop mismatches)))
  ;; The gist's counterexample: at T=10 with 0+4, 4+4, 8+1, 9+1, push merges
  ;; 8-9. Measuring from a pair's first message would merge 0-7 instead.
  (let ((view (make-array 4 :adjustable t :fill-pointer t
                            :initial-contents '((2 . 0) (2 . 1) (0 . 8) (0 . 9)))))
    (check (= 2 (maxwell::most-due-pair view 10 (constantly t))))
    (check (> (/ (- 10 0) 4) (/ (- 10 8) 1)))
    (maxwell::merge-view-pair view 2)
    (check (equal '((2 . 0) (2 . 1) (1 . 4)) (coerce view 'list))))
  ;; Pairs whose parent is not built are never merged.
  (let ((view (make-array 2 :adjustable t :fill-pointer t
                            :initial-contents '((0 . 0) (0 . 1)))))
    (check (null (maxwell::most-due-pair view 2 (constantly nil))))))

(defun drain-synchronously (chat model)
  (loop repeat 1000
        until (maxwell::compactor-idle-p chat)
        do (maxwell::compactor-tick chat nil :model-function model :tools #())))

(defun test-view-sawtooth ()
  (with-fresh-chat (chat)
    (let ((model (fake-line-model))
          (low (- maxwell::*view-low-water* 1100))
          (high (+ maxwell::*view-high-water* 600))
          (reached nil)
          (batches 0)
          (churned 0)
          (out-of-range 0)
          (previous '()))
      (dotimes (n 1000)
        (maxwell::chat-append chat :user (long-text 600) :time 0)
        (drain-synchronously chat model)
        (let* ((view (coerce (maxwell::chat-view chat) 'list))
               (size (maxwell::view-size chat (maxwell::chat-view chat))))
          (when (> size maxwell::*view-low-water*) (setf reached t))
          (when (and reached (not (<= low size high))) (incf out-of-range))
          (cond
            ((and (<= (length previous) (length view))
                  (equal previous (subseq view 0 (length previous)))))
            ((< (length view) (length previous)) (incf batches))
            (t (incf churned)))
          (setf previous view)))
      (check reached)
      (check (zerop out-of-range))
      (check (>= batches 2))
      ;; Between batches the view only grows at its end.
      (check (zerop churned))
      ;; The compaction view keeps its own, smaller sawtooth.
      (check (<= (maxwell::view-size chat (maxwell::chat-compaction-view chat))
                 (+ maxwell::*compaction-view-high-water* 600)))
      (check (maxwell::summarized-before-p chat (maxwell::chat-message-count chat)))
      (check (search "1000 messages" (maxwell::memory-report chat))))))

(defun test-tree-and-log ()
  (with-fresh-chat (chat)
    ;; Short messages are their own lines; two that fit are joined.
    (check (equal '(0) (maxwell::chat-append chat :user "hello" :time 0)))
    (check (equal '(1) (maxwell::chat-append chat :maxwell "hi there" :time 0)))
    (check (string= "user: hello" (maxwell::node-text (maxwell::chat-node chat 0 0))))
    (check (string= (format nil "user: hello~%maxwell: hi there")
                    (maxwell::node-text (maxwell::chat-node chat 1 0))))
    (check (string= (format nil "<chat>~%0+1|user: hello~%1+1|maxwell: hi there~%</chat>")
                    (maxwell::render-view chat)))
    (check (string= (format nil "<chat>~%0+1|user: hello~%</chat>")
                    (maxwell::render-view chat :end 1)))
    ;; Sizes are UTF-8 bytes.
    (maxwell::chat-append chat :user "é")
    (check (= 2 (maxwell::message-size (maxwell::logged-message chat 2))))
    (check (signals-error-p (lambda () (maxwell::chat-append chat :thought "x")))))
  (with-fresh-chat (chat)
    ;; Tool results are clipped to head and tail; other long text is split.
    (let* ((output (concatenate 'string "HEAD" (long-text 70000) "TAIL"))
           (ids (maxwell::chat-append chat :echo output)))
      (check (= 1 (length ids)))
      (let ((text (maxwell::message-text (maxwell::logged-message chat (first ids)))))
        (check (<= (length text) 30000))
        (check (string= "HEAD" (subseq text 0 4)))
        (check (string= "TAIL" (subseq text (- (length text) 4))))
        (check (search "clipped" text))))
    (let ((ids (maxwell::chat-append chat :user (long-text 70000 #\b))))
      (check (equal '(1 2 3) ids))
      (check (= 70000 (loop for id in ids
                            sum (length (maxwell::message-text
                                         (maxwell::logged-message chat id)))))))
    ;; Long leaves wait for the compactor; at most 8 are released ahead.
    (dotimes (n 20) (maxwell::chat-append chat :user (long-text 600)))
    (check (= 8 (length (maxwell::chat-ready chat))))
    (check (not (maxwell::summarized-before-p chat 1)))
    (check (string= (format nil "<chat>~%</chat>") (maxwell::render-view chat)))
    (check (search "24 messages" (maxwell::memory-report chat)))))

(defun scripted-model (lines &optional (requests (list '())))
  "A synchronous model answering with LINES in turn, recording each request's
messages in the first element of REQUESTS."
  (lambda (messages tools)
    (declare (ignore tools))
    (push messages (first requests))
    (fake-response :content (pop lines))))

(defun test-compactor-requests ()
  ;; The request reuses the turns' system prompt and tools exactly.
  (with-fresh-chat (chat)
    (let* ((requests (list '()))
           (tools (maxwell/json:json-array (maxwell/json:json-object "name" "t")))
           (seen-tools nil)
           (model (lambda (messages tools)
                    (push messages (first requests))
                    (setf seen-tools tools)
                    (fake-response :content "user: a long greeting"))))
      (maxwell::chat-append chat :user "short")
      (maxwell::chat-append chat :user (long-text 600))
      (maxwell::compactor-tick chat nil :model-function model :tools tools
                                        :system-prompt maxwell::*base-system-prompt*)
      (let ((messages (first (first requests))))
        (check (eq tools seen-tools))
        (check (= 3 (length messages)))
        (check (string= maxwell::*base-system-prompt*
                        (maxwell/json:get-key (first messages) "content")))
        (check (string= (format nil "<chat>~%0+1|user: short~%</chat>")
                        (maxwell/json:get-key (second messages) "content")))
        (let ((task (maxwell/json:get-key (third messages) "content")))
          (check (search "Compaction: compress message 1 into one line of at most 512 bytes"
                         task))
          (check (search (format nil "~%~A~%<input>" (long-text 512 #\-)) task))))
      (check (string= "user: a long greeting"
                      (maxwell::node-text (maxwell::chat-node chat 0 1))))))
  ;; Too long: retry in the same conversation, at most 5 times, keep the shortest.
  (with-fresh-chat (chat)
    (let* ((requests (list '()))
           (model (scripted-model (list (long-text 600 #\p) (long-text 530 #\q)
                                        (long-text 700 #\r) (long-text 515 #\s)
                                        (long-text 520 #\t) (long-text 100 #\u))
                                  requests)))
      (maxwell::chat-append chat :user (long-text 600))
      (maxwell::compactor-tick chat nil :model-function model :tools #())
      (check (= 5 (length (first requests))))
      (check (string= (long-text 515 #\s)
                      (maxwell::node-text (maxwell::chat-node chat 0 0))))
      (let* ((second-request (second (reverse (first requests))))
             (retry (maxwell/json:get-key (car (last second-request)) "content")))
        (check (= 5 (length second-request)))
        (check (string= "assistant"
                        (maxwell/json:get-key (fourth second-request) "role")))
        (check (search "Too long: your line is 600 bytes, over the 512-byte limit." retry))
        (check (search (format nil "~%~A| ← LIMIT" (long-text 512 #\p)) retry)))))
  ;; A line that fits ends the retries.
  (with-fresh-chat (chat)
    (let* ((requests (list '()))
           (model (scripted-model (list (long-text 600 #\p) "12+1|fits") requests)))
      (maxwell::chat-append chat :user (long-text 600))
      (maxwell::compactor-tick chat nil :model-function model :tools #())
      (check (= 2 (length (first requests))))
      ;; An id+n| head copied from the input is dropped.
      (check (string= "fits" (maxwell::node-text (maxwell::chat-node chat 0 0))))))
  ;; Merges get both lines and the context up to the merge's last message.
  (with-fresh-chat (chat)
    (let* ((requests (list '()))
           (model (scripted-model (list (long-text 300 #\a) (long-text 300 #\b) "merged")
                                  requests)))
      (maxwell::chat-append chat :user (long-text 600))
      (maxwell::chat-append chat :user (long-text 600))
      (drain-synchronously chat model)
      (let ((task (maxwell/json:get-key (third (first (first requests))) "content")))
        (check (search "Compaction: merge lines 0+1 and 1+1, adjacent" task))
        (check (search "<chat> may hold their messages, 0 to 1, in more detail" task)))
      (check (string= "merged" (maxwell::node-text (maxwell::chat-node chat 1 0)))))))

(defun test-compactor-jobs ()
  ;; At most 8 requests run at once; merges queue behind leaves.
  (with-fresh-chat (chat)
    (let* ((pending '())
           (model (lambda (messages tools)
                    (declare (ignore tools))
                    (let ((task (maxwell/json:get-key (car (last messages)) "content")))
                      (if (search "Compaction: compress" task)
                          (fake-response :content (long-text 300 #\l))
                          (let ((cell (list nil)))
                            (push cell pending)
                            (lambda () (first cell))))))))
      (dotimes (n 40) (maxwell::chat-append chat :user (long-text 600)))
      (dotimes (n 10) (maxwell::compactor-tick chat nil :model-function model :tools #()))
      (check (= 8 (hash-table-count (maxwell::chat-jobs chat))))
      (check (plusp (length (maxwell::chat-ready chat))))
      (check (maxwell::summarized-before-p chat 40))
      ;; Replies that arrive later are reaped on a later tick.
      (loop repeat 100
            until (maxwell::compactor-idle-p chat)
            do (dolist (cell pending)
                 (setf (first cell) (fake-response :content "pair")))
               (maxwell::compactor-tick chat nil :model-function model :tools #()))
      (check (maxwell::compactor-idle-p chat))
      (check (maxwell::chat-node chat 5 0))
      ;; Draining cancels what is left and requeues it.
      (maxwell::chat-append chat :user (long-text 600))
      (maxwell::compactor-tick chat nil :model-function (lambda (m tl)
                                                          (declare (ignore m tl))
                                                          (constantly nil))
                               :tools #())
      (check (= 1 (hash-table-count (maxwell::chat-jobs chat))))
      (check (= 1 (maxwell::compactor-drain chat 0)))
      (check (zerop (hash-table-count (maxwell::chat-jobs chat))))
      (check (equal '((0 . 40)) (maxwell::chat-ready chat)))))
  ;; A failed call is tried again after the next message.
  (with-fresh-chat (chat)
    (let* ((fail t)
           (model (lambda (messages tools)
                    (declare (ignore messages tools))
                    (if fail
                        (error "provider down")
                        (fake-response :content "built")))))
      (maxwell::chat-append chat :user (long-text 600))
      (maxwell::compactor-tick chat nil :model-function model :tools #())
      (check (null (maxwell::chat-node chat 0 0)))
      (check (equal '((0 . 0)) (maxwell::chat-retry chat)))
      (maxwell::compactor-tick chat nil :model-function model :tools #())
      (check (null (maxwell::chat-ready chat)))
      (setf fail nil)
      (maxwell::chat-append chat :user "next")
      (maxwell::compactor-tick chat nil :model-function model :tools #())
      (check (string= "built" (maxwell::node-text (maxwell::chat-node chat 0 0))))))
  ;; Compact model selection and its one-time warning.
  (let ((maxwell::*compact-model-warned* nil)
        (main (maxwell:make-client :model "main" :compact-model nil))
        (cheap (maxwell:make-client :model "main" :compact-model "cheap")))
    (check (string= "cheap" (maxwell:client-model (maxwell::compaction-client cheap))))
    (check (string= "main" (maxwell:client-model cheap)))
    (check (eq main (maxwell::compaction-client main)))
    (check (null (maxwell::compact-model-warning cheap)))
    (check (search "main" (maxwell::compact-model-warning main)))
    (check (null (maxwell::compact-model-warning main))))
  ;; The non-blocking command API that compaction jobs run on.
  (let ((job (maxwell::start-command '("cat") :input "async body")))
    (loop repeat 500 until (maxwell::command-finished-p job) do (sleep 0.01))
    (multiple-value-bind (output error-output status) (maxwell::finish-command job)
      (check (string= "async body" output))
      (check (string= "" error-output))
      (check (zerop status))))
  (let ((job (maxwell::start-command '("sleep" "5"))))
    (maxwell::cancel-command job)
    (check (maxwell::command-finished-p job))
    (check (not (probe-file (maxwell::command-job-input-path job)))))
  (check (= 1 (maxwell/json:get-key
                (maxwell::parse-provider-output (format nil "{\"x\":1}~%200") "" 0 nil)
                "x")))
  (check (signals-error-p
           (lambda () (maxwell::parse-provider-output (format nil "{}~%500") "" 0 nil)))))

(defun test-zoom-and-date ()
  (with-fresh-chat (chat)
    (let ((time (encode-universal-time 5 4 3 2 1 2026 0)))
      (maxwell::chat-append chat :user "alpha" :time time)
      (maxwell::chat-append chat :maxwell "beta" :time time)
      (maxwell::chat-append chat :tool "(+ 1 2)" :time time)
      (maxwell::chat-append chat :echo (long-text 600) :time time)
      (maxwell::chat-append chat :user (long-text 25000 #\z) :time time)
      (check (string= "0+1|user: alpha" (maxwell:zoom 0 1)))
      (check (string= (format nil "0+1|user: alpha~%1+1|maxwell: beta") (maxwell:zoom 0 2)))
      ;; Unbuilt lines say so; lines past the end are left out.
      (check (string= (format nil "2+1|tool: (+ 1 2)~%3+1|(not summarized yet: zoom it)")
                      (maxwell:zoom 2 2)))
      (check (string= "4+2|(not summarized yet: zoom it)" (maxwell:zoom 4 4)))
      ;; Long messages come in pages.
      (let ((first-page (maxwell:zoom 4 1))
            (last-page (maxwell:zoom 4 1 3)))
        (check (search "[page 1 of 3; next: (maxwell:zoom 4 1 2)]" first-page))
        (check (search "user: zzz" first-page))
        (check (search "[page 3 of 3]" last-page))
        (check (= 25006 (loop for page from 1 to 3
                              for text = (maxwell:zoom 4 1 page)
                              sum (- (length text) (1+ (position #\Newline text)))))))
      (dolist (arguments '((1 2) (0 3) (0 0) (-2 2) (100 1) ("0" 1) (0 1 2) (4 1 4)))
        (check (signals-error-p (lambda () (apply #'maxwell:zoom arguments)))))
      (check (string= "2026-01-02T03:04:05Z" (maxwell:date 1)))
      (dolist (id '(5 -1 "1" nil))
        (check (signals-error-p (lambda () (maxwell:date id)))))
      ;; Errors read well through COMMON_LISP.
      (check (search "power of two" (maxwell:eval-lisp "(maxwell:zoom 0 3)")))))
  (let ((maxwell:*chat* nil))
    (check (signals-error-p (lambda () (maxwell:zoom 0 1))))))

(defun test-migration ()
  (with-fresh-chat (chat)
    (let ((history
            (list (maxwell/json:json-object "role" "user" "content" "Fix it.")
                  (maxwell/json:json-object
                    "role" "assistant" "content" maxwell/json:+null+
                    "tool_calls" (maxwell/json:json-array (tool-call "(fix)")))
                  (maxwell/json:json-object "role" "tool" "tool_call_id" "call-1"
                                            "content" "FIXED")
                  (maxwell/json:json-object "role" "assistant" "content" "Done."))))
      (check (= 4 (maxwell::migrate-legacy-history chat history)))
      (check (equal '(:user :tool :echo :maxwell)
                    (map 'list #'maxwell::message-kind (maxwell::chat-messages chat))))
      (check (equal '("Fix it." "(fix)" "FIXED" "Done.")
                    (map 'list #'maxwell::message-text (maxwell::chat-messages chat))))
      (check (string= "0+1|user: Fix it." (maxwell:zoom 0 1))))))

(defun test-project-tags ()
  (let ((base (scratch-directory "projects")))
    (unwind-protect
         (with-fresh-chat (chat)
           (let ((first (merge-pathnames "a/proj/" base))
                 (second (merge-pathnames "b/proj/" base))
                 (repository (merge-pathnames "repo/" base))
                 (nested (merge-pathnames "repo/src/deep/" base)))
             (mapc #'ensure-directories-exist (list first second nested))
             (ensure-directories-exist (merge-pathnames ".git/" repository))
             ;; The git root names the project, from any directory under it.
             (check (string= "repo" (maxwell::workspace-project chat nested)))
             (check (string= "repo" (maxwell::workspace-project chat repository)))
             (when (equal (maxwell::project-root first) first)
               ;; A second root with a taken name gets parent directories.
               (check (string= "proj" (maxwell::workspace-project chat first)))
               (check (string= "b/proj" (maxwell::workspace-project chat second)))
               (check (string= "proj" (maxwell::workspace-project chat first)))
               (maxwell::chat-append chat :user "x" :workspace first)
               (maxwell::chat-append chat :user "y" :workspace second)
               (check (string= (format nil "0+1|[proj] user: x")
                               (maxwell::line-text chat 0 0)))
               ;; Tags count toward the view's size but not the node's.
               (check (= (1+ (length "0+1|[proj] user: x"))
                         (maxwell::line-size chat '(0 . 0))))
               (check (= (length "user: x") (maxwell::node-size (maxwell::chat-node chat 0 0))))
               ;; A merged node carries the union, oldest first.
               (check (equal '("proj" "b/proj")
                             (maxwell::node-projects (maxwell::chat-node chat 1 0))))
               (check (string= "0+2|[proj,b/proj] user: x user: y"
                               (maxwell::line-text chat 1 0))))))
      (uiop:delete-directory-tree base :validate t :if-does-not-exist :ignore)))
  (check (string= "" (maxwell::project-tag '())))
  (check (string= "[a,b,c] " (maxwell::project-tag '("a" "b" "c"))))
  (check (string= "[a,b,c,+2] " (maxwell::project-tag '("a" "b" "c" "d" "e")))))

(defun test-agents-md-import ()
  (let ((base (scratch-directory "agents")))
    (unwind-protect
         (with-fresh-chat (chat)
           (let ((file (merge-pathnames "AGENTS.md" base)))
             (ensure-directories-exist base)
             (ensure-directories-exist (merge-pathnames ".git/" base))
             (check (null (maxwell::import-agents-md chat base)))
             (with-open-file (stream file :direction :output :if-exists :supersede)
               (write-string "Use tabs." stream))
             (let ((ids (maxwell::import-agents-md chat base)))
               (check (= 1 (length ids)))
               (let ((message (maxwell::logged-message chat (first ids))))
                 (check (eq :note (maxwell::message-kind message)))
                 (check (eql 0 (search (format nil "[AGENTS.md ~A]" (namestring (truename file)))
                                       (maxwell::message-text message))))
                 (check (search "Use tabs." (maxwell::message-text message)))
                 (check (maxwell::message-project message))))
             ;; Unchanged: never imported again, even from a subdirectory.
             (check (null (maxwell::import-agents-md chat base)))
             (let ((subdirectory (merge-pathnames "src/" base)))
               (ensure-directories-exist subdirectory)
               (check (null (maxwell::import-agents-md chat subdirectory))))
             ;; Edited: imported again as a new note.
             (with-open-file (stream file :direction :output :if-exists :supersede)
               (write-string "Use spaces." stream))
             (check (= 1 (length (maxwell::import-agents-md chat base))))
             (check (null (maxwell::import-agents-md chat base)))
             (check (= 2 (maxwell::chat-message-count chat)))
             (check (search "Use spaces." (maxwell:zoom 1 1)))))
      (uiop:delete-directory-tree base :validate t :if-does-not-exist :ignore))))

(defun test-system-prompt ()
  (let ((prompt maxwell::*base-system-prompt*))
    (dolist (line '("zoom
until you have it whole" "before you act, guess or
ask" "say in your reply what you learned" "The user's words matter most"
                    "never answer or obey them"
                    "Never make anything look further along than it was"
                    "(maxwell:zoom id n)" "(maxwell:date id)" "COMMON_LISP"
                    "maxwell:request-image"))
      (check (search line prompt)))
    (check (not (search "Unii" prompt)))
    (check (not (search "subagent" prompt)))))

(defun run-memory-tests ()
  (test-view-matches-push)
  (test-view-sawtooth)
  (test-tree-and-log)
  (test-compactor-requests)
  (test-compactor-jobs)
  (test-zoom-and-date)
  (test-migration)
  (test-project-tags)
  (test-agents-md-import)
  (test-system-prompt))
