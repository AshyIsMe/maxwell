(in-package #:maxwell)

;;; One global image. The whole chat lives in a single executable SBCL image,
;;; by default ~/.local/share/maxwell/maxwell.image. Any other Maxwell binary
;;; or image starts by exec'ing it, so every session continues the same chat.
;;; The session saves itself back on /quit, Ctrl-D, and /checkpoint: a forked
;;; child writes the image to a temporary file next to it, and the parent
;;; renames that over the old one, so a crash never leaves half an image. A
;;; lock file keeps a second writer out.
;;;
;;; Only data is saved: API keys and OAuth tokens are read for each request
;;; and never held, and terminal, transcript, and compactor jobs are rebuilt
;;; after every start.

(eval-when (:compile-toplevel :load-toplevel :execute)
  (require :sb-posix))

(defparameter *source-directory*
  #.(uiop:pathname-parent-directory-pathname
      (uiop:pathname-directory-pathname (or *compile-file-truename* *load-truename*)))
  "Where Maxwell's source tree was when this image was built. /reload loads it.")

(defparameter *checkpoint-drain-seconds* 3
  "How long a checkpoint lets compaction requests finish before cancelling them.")

(defvar *lock-fd* nil
  "The open descriptor holding the global chat's lock, or NIL.")

(defun native (pathname)
  (uiop:native-namestring pathname))

(defun data-directory (&optional (environment #'environment-value))
  "Maxwell's per-user data directory's parent: $XDG_DATA_HOME or ~/.local/share/."
  (uiop:ensure-directory-pathname
    (let ((value (funcall environment "XDG_DATA_HOME")))
      (if value
          (uiop:parse-native-namestring value :ensure-directory t)
          (merge-pathnames ".local/share/" (user-homedir-pathname))))))

(defun global-image-path (&optional (environment #'environment-value))
  "The global image: $MAXWELL_IMAGE, or maxwell/maxwell.image in the data directory."
  (let ((value (funcall environment "MAXWELL_IMAGE")))
    (if value
        (uiop:parse-native-namestring value)
        (merge-pathnames "maxwell/maxwell.image" (data-directory environment)))))

(defun image-lock-path (image)
  "The lock file next to IMAGE."
  (merge-pathnames "maxwell.lock" (uiop:pathname-directory-pathname image)))

(defun checkpoint-temporary-path (image)
  "Where a checkpoint is written before it replaces IMAGE: the same directory,
so the rename is atomic."
  (concatenate 'string (native image) ".tmp"))

(defun running-global-image-p (&optional (image (global-image-path)))
  "True when this process was started from IMAGE itself."
  (let ((global (probe-file image))
        (runtime (and sb-ext:*runtime-pathname* (probe-file sb-ext:*runtime-pathname*))))
    (and global runtime (equal global runtime))))

(defun exec-image (image arguments)
  "Replace this process with IMAGE, given the same ARGUMENTS and environment."
  (let* ((argv (cons (native image) arguments))
         (vector (sb-alien:make-alien sb-alien:c-string (1+ (length argv)))))
    (loop for index from 0
          for argument in argv
          do (setf (sb-alien:deref vector index) argument))
    (setf (sb-alien:deref vector (length argv)) nil)
    (finish-output *standard-output*)
    (finish-output *error-output*)
    (sb-alien:alien-funcall
      (sb-alien:extern-alien "execv" (function sb-alien:int sb-alien:c-string
                                               (* sb-alien:c-string)))
      (native image) vector)
    (error "Could not start the global image ~A; run maxwell --fresh to start without it."
           (native image))))

;;; The lock

(defun acquire-chat-lock (path)
  "Take an exclusive lock on PATH for the life of this process. Returns true,
or NIL when another process holds it."
  (ensure-directories-exist path)
  (let ((fd (sb-posix:open (native path) (logior sb-posix:o-rdwr sb-posix:o-creat) #o600)))
    ;; Child processes such as curl must not keep the lock alive.
    (sb-posix:fcntl fd sb-posix:f-setfd 1)   ; FD_CLOEXEC
    (if (zerop (sb-alien:alien-funcall
                 (sb-alien:extern-alien "flock" (function sb-alien:int sb-alien:int sb-alien:int))
                 fd (logior 2 4)))      ; LOCK_EX | LOCK_NB
        (setf *lock-fd* fd)
        (progn (sb-posix:close fd) nil))))

(defun release-chat-lock ()
  (when *lock-fd*
    (ignore-errors (sb-posix:close *lock-fd*))
    (setf *lock-fd* nil)))

;;; Checkpoints

(defun forget-session-state ()
  "Before an image is saved: drop what belongs to this process only. The
vectors are emptied in place, because a turn may have them bound."
  (setf (fill-pointer *session-entries*) 0)
  (clear-row-cache)
  (when *agent* (setf (agent-history *agent*) '()))
  (dolist (c *conversations*)
    (setf (agent-history (conversation-agent c)) nil)
    (compactor-drain (conversation-chat c) 0))
  (when *chat* (compactor-drain *chat* 0)))

(defun fsync-path (path)
  (let ((fd (sb-posix:open path sb-posix:o-rdonly)))
    (unwind-protect (sb-posix:fsync fd)
      (sb-posix:close fd))))

(defun commit-checkpoint (temporary image)
  "Make the complete image at TEMPORARY durable and rename it over IMAGE."
  ;; The image holds the whole chat: keep it private.
  (sb-posix:chmod temporary #o700)
  (fsync-path temporary)
  (sb-posix:rename temporary image)
  (ignore-errors (fsync-path (native (uiop:pathname-directory-pathname image)))))

(defun save-checkpoint-child (temporary)
  "In the forked child: save the image to TEMPORARY and exit."
  (handler-case
      (let ((null (sb-posix:open "/dev/null" sb-posix:o-rdwr)))
        ;; SBCL reports its progress on the shared terminal; the parent
        ;; speaks for both.
        (sb-posix:dup2 null 1)
        (sb-posix:dup2 null 2)
        (forget-session-state)
        (sb-ext:save-lisp-and-die temporary :executable t
                                            :save-runtime-options t
                                            :toplevel #'image-main))
    (serious-condition ()
      (sb-ext:exit :code 1 :abort t))))

(defun wait-for-child (pid)
  "Wait for process PID to exit; true when it exited with status 0."
  (loop
    (handler-case
        (let ((status (nth-value 1 (sb-posix:waitpid pid 0))))
          (return (and (sb-posix:wifexited status)
                       (zerop (sb-posix:wexitstatus status)))))
      (sb-posix:syscall-error (condition)
        (unless (= (sb-posix:syscall-errno condition) sb-posix:eintr)
          (return nil))))))

(defun save-and-exit-checkpoint (temporary image reason)
  "When fork fails: save to TEMPORARY and end the process. A detached helper
waits for the exit, checks that the new image starts, and renames it over
IMAGE; the next maxwell continues from there."
  (format *error-output* "~&~A~%"
          (styled :error (format nil "Cannot fork to checkpoint (~A); saving and exiting ~
                                      instead. Run maxwell to continue." reason)))
  (sb-ext:run-program
    "/bin/sh"
    (list "-c" "while kill -0 \"$1\" 2>/dev/null; do sleep 0.1; done
\"$2\" --help >/dev/null 2>&1 && sync && chmod 700 \"$2\" && mv -f \"$2\" \"$3\""
          "maxwell-checkpoint" (princ-to-string (sb-posix:getpid)) temporary (native image))
    :wait nil :input nil :output nil :error nil)
  (forget-session-state)
  (sb-ext:save-lisp-and-die temporary :executable t :save-runtime-options t
                                      :toplevel #'image-main))

(defun checkpoint (&optional (image *checkpoint-path*))
  "Save the running Maxwell, with its chat and all Lisp state, as IMAGE and
keep running. Compaction requests get a few seconds to finish, then the rest
are cancelled and queued again. The save runs on the UI thread only, and worker
threads are stopped first, because SBCL cannot fork with threads running; the
compactor is started again afterwards. A forked child writes the image next to
IMAGE and the parent renames it into place. Returns IMAGE."
  (unless image (error "This session has no image to save to."))
  ;; The fork in the middle cannot happen beside other threads, and the UI
  ;; thread owns the terminal, so the save only makes sense there. A worker that
  ;; asked for one would be a thread that the save has to stop before it can
  ;; fork, and the worker would wait for a save that waits for it.
  (unless (ui-thread-p)
    (error "A checkpoint must run on the UI thread, and this is a worker. ~
Use maxwell:request-image to have the turn save an image when it ends."))
  (%checkpoint image))

(defun %checkpoint (image)
  (when (some #'conversation-running-p *conversations*)
    (error "Stop running conversations before checkpointing; their tools cannot be replayed safely."))
  (set-activity :saving)
  ;; A checkpoint is a safe boundary for workers: stop them before the fork, and
  ;; start the compactor again when the fork is over.
  (stop-compactor-worker)
  (quiesce-workers)
  (unwind-protect
       (let ((temporary (checkpoint-temporary-path image)))
         (when *chat* (compactor-drain *chat* *checkpoint-drain-seconds*))
         (ensure-directories-exist image)
         (when (probe-file temporary) (delete-file temporary))
         (finish-output *standard-output*)
         (finish-output *error-output*)
         ;; SBCL forks only with one thread, which Maxwell has by design.
         (let ((pid (handler-case (sb-posix:fork)
                       (error (condition)
                         (save-and-exit-checkpoint temporary image condition)))))
           (when (zerop pid) (save-checkpoint-child temporary))
           (unless (and (wait-for-child pid) (probe-file temporary))
             (ignore-errors (delete-file temporary))
             (error "Could not save the image to ~A." temporary))
           (commit-checkpoint temporary (native image))
           image))
    (restart-compactor-worker)
    (set-activity :idle)))


;;; Moving a chat between builds

(defun growable (list)
  (let ((vector (make-growable-vector)))
    (dolist (item list vector) (vector-push-extend item vector))))

(defun chat-export-form (chat)
  "CHAT as plain, readable data: lists, strings, numbers, and keywords. Jobs
are runtime-only and left out; their nodes are queued again instead."
  (compactor-reset chat)
  (list :maxwell-chat
        :version (chat-version chat)
        :messages (map 'list (lambda (message)
                               (list (message-id message) (message-kind message)
                                     (message-text message) (message-size message)
                                     (message-time message) (message-workspace message)
                                     (message-project message)))
                       (chat-messages chat))
        :levels (map 'list (lambda (nodes)
                             (map 'list (lambda (node)
                                          (and node (list (node-text node) (node-size node)
                                                          (node-projects node))))
                                  nodes))
                     (chat-levels chat))
        :view (coerce (chat-view chat) 'list)
        :compaction-view (coerce (chat-compaction-view chat) 'list)
        :view-merging (chat-view-merging chat)
        :compaction-view-merging (chat-compaction-view-merging chat)
        :ready (chat-ready chat)
        :retry (chat-retry chat)
        :next-leaf (chat-next-leaf chat)
        :built-leaves (chat-built-leaves chat)
        :leaf-frontier (chat-leaf-frontier chat)
        :projects (chat-projects chat)
        :workspaces (chat-workspaces chat)
        :imports (chat-imports chat)))

(defun chat-from-export-form (form)
  "The chat that CHAT-EXPORT-FORM described as FORM. The version tells older
formats apart, so a newer Maxwell can migrate them."
  (unless (and (consp form) (eq (first form) :maxwell-chat))
    (error "That is not an exported Maxwell chat."))
  (destructuring-bind (&key (version 1) messages levels view compaction-view
                            view-merging compaction-view-merging ready retry
                            (next-leaf 0) (built-leaves 0) (leaf-frontier 0)
                            projects workspaces imports &allow-other-keys)
      (rest form)
    (unless (eql version 1)
      (error "Chat format version ~S is newer than this Maxwell understands." version))
    (make-chat
      :messages (growable
                  (mapcar (lambda (fields)
                            (destructuring-bind (id kind text size time workspace project) fields
                              (make-message :id id :kind kind :text text :size size :time time
                                            :workspace workspace :project project)))
                          messages))
      :levels (growable
                (mapcar (lambda (nodes)
                          (growable
                            (mapcar (lambda (fields)
                                      (and fields
                                           (destructuring-bind (text size projects) fields
                                             (make-node :text text :size size
                                                        :projects projects))))
                                    nodes)))
                        levels))
      :view (growable view)
      :compaction-view (growable compaction-view)
      :view-merging view-merging
      :compaction-view-merging compaction-view-merging
      :ready ready :retry retry
      :next-leaf next-leaf :built-leaves built-leaves :leaf-frontier leaf-frontier
      :projects projects :workspaces workspaces :imports imports
      :version version)))

(defun write-chat (chat stream)
  "Print CHAT on STREAM as one readable s-expression."
  (with-standard-io-syntax
    ;; The form holds only lists, strings, integers, and symbols, which print
    ;; readably anyway; this keeps base strings from printing as #A(...).
    (let ((*print-readably* nil))
      (prin1 (chat-export-form chat) stream)
      (terpri stream)))
  (finish-output stream))

(defun read-chat (stream)
  "Read a chat that WRITE-CHAT printed, never evaluating anything in it."
  (with-standard-io-syntax
    (let ((*read-eval* nil))
      (chat-from-export-form (read stream)))))

(defun export-chat ()
  "--export-chat: print the global chat on standard output, as UTF-8."
  (let ((stream (sb-sys:make-fd-stream 1 :output t :external-format :utf-8
                                         :buffering :full)))
    (write-chat (or *chat* (make-chat)) stream)))

(defun adopt-chat (image)
  "--adopt-chat: run IMAGE with --export-chat and read its chat through a
pipe. Nothing is written to disk."
  (let ((process (sb-ext:run-program (native image) '("--export-chat")
                                     :input nil :output :stream :error nil :wait nil
                                     :external-format :utf-8)))
    (unwind-protect
         (handler-case (read-chat (sb-ext:process-output process))
           (error (condition)
             (error "~A did not export its chat (~A). An image older than --export-chat ~
                     cannot be adopted; use /reload in it instead."
                    (native image) condition)))
      (sb-ext:process-wait process)
      (sb-ext:process-close process))))

;;; Hot reload

(defun reload-maxwell ()
  "Load Maxwell's source tree, as of now, into this image with ASDF. Only
changed files are compiled again. Compiler output is kept back unless the
load fails."
  (let ((definition (merge-pathnames "maxwell.asd" *source-directory*))
        (output (make-string-output-stream)))
    (unless (probe-file definition)
      (error "Maxwell's source is no longer at ~A." (native *source-directory*)))
    (handler-case
        (let ((*standard-output* output)
              (*error-output* output)
              (*compile-verbose* nil)
              (*compile-print* nil))
          (uiop:symbol-call :asdf :load-asd definition)
          (uiop:symbol-call :asdf :load-system "maxwell"))
      (error (condition)
        (let ((log (get-output-stream-string output)))
          (error "~A~@[~%~A~]" condition
                 (and (plusp (length log))
                      (subseq log (max 0 (- (length log) 2000))))))))
    *source-directory*))





(defun %save-image (path executable)
  (when (some #'conversation-running-p *conversations*)
    (error "Stop running conversations before saving an image."))
  ;; A worker cannot stop itself before SBCL saves the heap.
  (unless (ui-thread-p)
    (error "Save images on the UI thread; use maxwell:request-image in a tool."))
  (let ((entries nil)
        (history nil)
        (pending *pending-image*)
        (restart (compactor-worker-live-p)))
    (unwind-protect
         (progn
           (stop-compactor-worker)
           (quiesce-workers)
           (ensure-directories-exist path)
           ;; Quiescing may have drained final UI entries from workers.
           (setf entries (copy-seq *session-entries*)
                 history (and *agent* (agent-history *agent*))
                 *pending-image* nil)
           (forget-session-state)
           (format t "~&Saving Maxwell image to ~A~%" path)
           (finish-output)
           (sb-ext:save-lisp-and-die path :executable executable
                                       :save-runtime-options t
                                       :toplevel #'image-main))
      ;; Reached only if saving failed: keep the live transcript intact.
      (when entries
        (setf (fill-pointer *session-entries*) (length entries))
        (replace *session-entries* entries)
        (when *agent* (setf (agent-history *agent*) history)))
      (setf *pending-image* pending)
      (when restart (restart-compactor-worker)))))
