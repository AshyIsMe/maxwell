(in-package #:maxwell)

;;; A fresh build reads versioned DATA, not old structure instances. The old
;;; executable image is retained as rollback, including its live Lisp code.
(defparameter *resume-state-version* 1)
(defparameter *stat-state-fields*
  '(conversation-id model wire-api started latency total prompt-tokens completion-tokens
    reasoning-tokens total-tokens tokens-per-second estimated))

(defun client-state (client)
  (list :base-url (client-base-url client) :endpoint (client-endpoint client)
        :wire-api (client-wire-api client) :model (client-model client)
        :compact-model (client-compact-model client)
        :api-key-environment (client-api-key-environment client)
        :auth-provider (client-auth-provider client)
        :extra-headers (client-extra-headers client)
        :request-fields (encode (or (client-request-fields client) (json-object)))
        :timeout (client-timeout client)))

(defun client-from-state (form)
  (let* ((fields (decode (getf form :request-fields)))
         (client (apply #'%make-client (append (list :request-fields fields)
                           (loop for (k v) on form by #'cddr
                                 unless (eq k :request-fields) append (list k v))))))
    client))

(defun resume-state-form ()
  (when (some #'conversation-running-p *conversations*)
    (error "Stop running conversations before restart; tool side effects are never replayed."))
  (with-chat-lock
    (list :maxwell-state :version *resume-state-version*
      :selected (conversation-id *selected-conversation*)
      :next-conversation-id *next-conversation-id* :next-note-id *next-note-id*
      :conversations
      (mapcar (lambda (c)
        (list :id (conversation-id c) :name (conversation-name c)
          :parent (conversation-parent c) :state (conversation-state c)
          :draft (if (eq c *selected-conversation*) *prompt-draft* (conversation-draft c))
          :client (client-state (agent-client (conversation-agent c)))
          :workspace (namestring (agent-workspace (conversation-agent c)))
          :system-prompt (agent-system-prompt (conversation-agent c))
          :chat (chat-export-form (conversation-chat c))
          :inbox (mapcar (lambda (n)
                   (list (conversation-note-id n) (conversation-note-from n)
                         (conversation-note-text n) (conversation-note-wake n)
                         (conversation-note-delivered n))) (conversation-inbox c))))
        *conversations*)
      :stats (mapcar (lambda (stat)
               (mapcar (lambda (field) (slot-value stat field)) *stat-state-fields*))
             *request-stats*))))

(defun restore-resume-state (form)
  (unless (and (consp form) (eq (first form) :maxwell-state)
               (eql (getf (rest form) :version) *resume-state-version*))
    (error "Unsupported Maxwell resume state."))
  (let* ((data (rest form))
         ;; Build everything before replacing the current registry.
         (conversations
           (mapcar (lambda (fields)
             (let ((c (%make-conversation
                      :id (getf fields :id) :name (getf fields :name)
                      :parent (getf fields :parent) :state (getf fields :state)
                      :draft (getf fields :draft)
                      :agent (make-agent :client (client-from-state (getf fields :client))
                               :workspace (getf fields :workspace)
                               :system-prompt (getf fields :system-prompt))
                      :chat (chat-from-export-form (getf fields :chat)))))
               (setf (conversation-inbox c)
                 (mapcar (lambda (n) (destructuring-bind (id from text wake delivered) n
                           (make-conversation-note :id id :from from :text text
                                                   :wake wake :delivered delivered)))
                         (getf fields :inbox)))
               c)) (getf data :conversations)))
         (selected (find (getf data :selected) conversations :key #'conversation-id)))
    (unless (and selected
                 (= (length conversations)
                    (length (remove-duplicates conversations :key #'conversation-name :test #'equal)))
                 (= (length conversations)
                    (length (remove-duplicates conversations :key #'conversation-id))))
      (error "Resume state has invalid conversation identities."))
    (setf *conversations* conversations *selected-conversation* selected
          *next-conversation-id* (getf data :next-conversation-id)
          *next-note-id* (getf data :next-note-id)
          *request-stats*
          (mapcar (lambda (values)
                    (let ((stat (make-request-stat)))
                      (loop for field in *stat-state-fields* for value in values
                            do (setf (slot-value stat field) value)) stat))
                  (getf data :stats)))
    (reset-conversation-runtime)
    (select-conversation (conversation-name selected) :draw nil)
    (length conversations)))

(defun write-resume-state (path)
  (with-open-stream (stream (open-private-output path))
    (with-standard-io-syntax
      (let ((*print-readably* t)) (prin1 (resume-state-form) stream) (terpri stream)))
    (finish-output stream))
  (fsync-path (native path))
  path)

(defun read-resume-state (path)
  (with-open-file (stream path)
    (with-standard-io-syntax
      (let ((*read-eval* nil))
        (let ((form (read stream)))
          (unless (eq :eof (read stream nil :eof)) (error "Trailing resume state data."))
          (restore-resume-state form))))))

(defun restart-build-command ()
  (list "sbcl" "--non-interactive" "--eval" "(require :asdf)"
        "--eval" (format nil "(asdf:load-asd (truename ~S))"
                         (native (merge-pathnames "maxwell.asd" *source-directory*)))
        "--eval" "(asdf:test-system :maxwell)"
        "--eval" "(asdf:make :maxwell)"))

(defun checked-restart-process (arguments)
  (multiple-value-bind (out err status)
      (uiop:run-program arguments :output :string :error-output :string
                         :ignore-error-status t :directory *source-directory*)
    (unless (zerop status)
      (let ((log (concatenate 'string out err)))
        (error "Candidate verification failed (~D):~%~A" status
               (subseq log (max 0 (- (length log) 4000))))))
    t))

(defun private-copy-image (source destination)
  (let* ((temporary (pathname (format nil "~A.tmp-~A" (native destination) (random-value 6))))
         (fd (sb-posix:open (native temporary)
                           (logior sb-posix:o-wronly sb-posix:o-creat sb-posix:o-excl) #o700)))
    (unwind-protect
         (progn
           (with-open-file (input source :element-type '(unsigned-byte 8))
             (with-open-stream (output (sb-sys:make-fd-stream fd :output t
                                         :element-type '(unsigned-byte 8) :auto-close t))
               (setf fd nil)
               (let ((buffer (make-array 65536 :element-type '(unsigned-byte 8))))
                 (loop for n = (read-sequence buffer input) while (plusp n)
                       do (write-sequence buffer output :end n)))
               (finish-output output)))
           (commit-checkpoint (native temporary) (native destination)))
      (when fd (ignore-errors (sb-posix:close fd)))
      (when (probe-file temporary) (delete-file temporary))))
  destination)

(defun prepare-restart (candidate state)
  "Build/test the candidate and boot it against the exact resume data."
  (checked-restart-process (restart-build-command))
  (private-copy-image (merge-pathnames "maxwell" *source-directory*) candidate)
  (sb-posix:chmod (native candidate) #o700)
  (checked-restart-process (list (native candidate) "--verify-resume" (native state)))
  candidate)

(defun restart-command ()
  "Verified restart at an idle boundary; failures leave this process usable."
  (unless *checkpoint-path* (error "Restart requires a persistent session."))
  (when (some #'conversation-running-p *conversations*)
    (error "Stop running conversations before restarting."))
  (ensure-conversations)
  (let* ((image *checkpoint-path*)
         (directory (uiop:pathname-directory-pathname image))
         (token (random-value 8))
         (state (merge-pathnames (format nil "resume-~A.sexp" token) directory))
         (candidate (merge-pathnames (format nil "candidate-~A" token) directory))
         (rollback (pathname (concatenate 'string (native image) ".rollback")))
         (worker nil) (failure nil) (done nil))
    (stop-compactor-worker)
    (unwind-protect
         (progn
           ;; Snapshot the complete old runtime first, before building anything.
           (checkpoint image)
           (stop-compactor-worker)
           (write-resume-state state)
           (status "Building and verifying a replacement; current image remains unchanged.")
           (setf worker (start-worker "maxwell restart verification"
                          (lambda ()
                            (unwind-protect
                                 (handler-case (prepare-restart candidate state)
                                   (error (e) (setf failure e)))
                              (setf done t)))))
           (loop until done do (drain-ui-queue) (sleep 0.02))
           (worker-join worker)
           (when failure (error failure))
           (private-copy-image image rollback)
           (sb-posix:chmod (native rollback) #o700)
           (fsync-path (native rollback))
           (status "Verified. Restarting; rollback: ~A" (native rollback))
           ;; FD_CLOEXEC releases the old process's lock during exec, not before.
           ;; --fresh prevents an accidental jump back into the old global image.
           (exec-image candidate (list "--fresh" "--resume-state" (native state))))
      (when (worker-alive-p worker)
        (bt:destroy-thread (worker-thread worker)) (worker-join worker))
      (restart-compactor-worker))))
