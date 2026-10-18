(in-package #:maxwell)

;;; OptChat: one endless chat, a tree of summaries, and a fixed-size view.
;;;
;;; This follows VictorTaelin's UniiChat/OptMem design. Every message is kept
;;; word for word in an append-only log. A background compactor (compactor.lisp)
;;; compresses the log into a binary tree of one-line summaries: node(0, i) is
;;; message i in at most 512 bytes, and node(l, i) merges node(l-1, 2i) and
;;; node(l-1, 2i+1). The model sees the *view*, a list of nodes covering the
;;; whole chat, fine near now and coarse in the past, 64-128 KB in all.
;;;
;;; Everything is plain Lisp data reachable from *CHAT*, so SAVE-LISP-AND-DIE
;;; keeps it with nothing else. Only the compactor's in-flight jobs refer to
;;; running processes; COMPACTOR-DRAIN clears them before a checkpoint. Sizes
;;; are UTF-8 bytes, never tokens.

(defconstant +node-limit+ 512
  "The most bytes a summary line may hold, not counting its id and tags.")

(defparameter *view-high-water* 128000
  "Once the view's lines pass this many bytes, one batch merges them down.")
(defparameter *view-low-water* 64000
  "The size a merge batch shrinks the view to.")
(defparameter *compaction-view-high-water* 32000
  "Once a compaction's context view passes this many bytes, it merges down.")
(defparameter *compaction-view-low-water* 16000
  "The size the compaction view is merged down to.")

(defparameter *message-limit* 30000
  "The most characters one logged message holds. Longer tool results are
clipped to their head and tail; any other long text is split.")

(defparameter *leaf-lookahead* 8
  "A message's node is queued once fewer than this many earlier lines are unbuilt.")

(defparameter *zoom-page-size* 10000
  "Characters per page when ZOOM returns a long message.")

(defparameter *message-kinds* '(:user :maxwell :tool :echo :work :note)
  "The kinds of logged message: the user's words, Maxwell's replies, its tool
calls, tool results, a subagent's report, and imported memories.")

(defun make-growable-vector ()
  (make-array 0 :adjustable t :fill-pointer t))

(defstruct message
  "One logged message. ID is its index in the log and never changes."
  id kind text size time workspace project)

(defstruct node
  "One built summary line. PROJECTS lists the tags of the messages it covers,
oldest first."
  text size projects)

(defstruct chat
  "The whole OptChat state: the log, the summary tree, both views, and the
compactor's queues."
  (messages (make-growable-vector))          ; vector of MESSAGE, index = id
  (levels (make-growable-vector))            ; levels[l] = vector of NODE or NIL
  (view (make-growable-vector))              ; vector of (l . i), oldest first
  (compaction-view (make-growable-vector))   ; same shape, 16-32 KB sawtooth
  ;; True while a batch has not reached its low-water mark yet, so that it
  ;; continues merging what it can at each later message.
  view-merging
  compaction-view-merging
  (ready '())                                ; (l . i) ready to build, oldest first
  (retry '())                                ; failed (l . i), requeued at the next message
  (next-leaf 0)                              ; first message whose leaf is not yet released
  (built-leaves 0)                           ; leaves built so far (all < NEXT-LEAF)
  (leaf-frontier 0)                          ; every leaf below this index is built
  (projects '())                             ; (name . root namestring)
  (workspaces '())                           ; (workspace namestring . project name)
  (imports '())                              ; (project . sha256) already imported
  ;; In-flight compactor jobs, (l . i) -> job. They refer to live processes,
  ;; so they are runtime-only: COMPACTOR-DRAIN empties this before a save.
  (jobs (make-hash-table :test 'equal))
  (version 1))

(defvar *chat* nil
  "The global OptChat that Maxwell's turns read and write. ZOOM and DATE read it.")

(defun octet-length (string)
  (length (sb-ext:string-to-octets string :external-format :utf-8)))

(defun chat-message-count (chat)
  (length (chat-messages chat)))

(defun logged-message (chat id)
  "Message ID of CHAT, or NIL."
  (let ((messages (chat-messages chat)))
    (and (integerp id) (<= 0 id) (< id (length messages)) (aref messages id))))

;;; Projects

(defun directory-namestring* (pathname)
  (namestring (uiop:ensure-directory-pathname pathname)))

(defun project-root (workspace)
  "The git root containing WORKSPACE, or WORKSPACE itself when there is none."
  (let ((start (uiop:ensure-directory-pathname workspace)))
    (loop for directory = start then parent
          for parent = (uiop:pathname-parent-directory-pathname directory)
          do (when (or (uiop:directory-exists-p (merge-pathnames ".git/" directory))
                       (uiop:file-exists-p (merge-pathnames ".git" directory)))
               (return directory))
             (when (equal parent directory) (return start)))))

(defun project-name-candidates (root)
  "Names for ROOT from its last directory outward, ending with its full path."
  (let ((parts (reverse (remove-if-not #'stringp (rest (pathname-directory root))))))
    (append (loop for count from 1 to (length parts)
                  collect (format nil "~{~A~^/~}" (reverse (subseq parts 0 count))))
            (list (directory-namestring* root)))))

(defun workspace-project (chat workspace)
  "The project tag for messages from WORKSPACE, or NIL without a workspace.
A project is named after its git root (or the workspace directory). A second
root with a name already taken gets parent directories until the names
differ; names once given never change, so old view lines stay stable."
  (when workspace
    (let* ((key (directory-namestring* workspace))
           (cached (assoc key (chat-workspaces chat) :test #'string=)))
      (if cached
          (cdr cached)
          (let* ((root (directory-namestring* (project-root workspace)))
                 (name (or (car (rassoc root (chat-projects chat) :test #'string=))
                           (let ((name (find-if (lambda (candidate)
                                                  (not (assoc candidate (chat-projects chat)
                                                              :test #'string=)))
                                                (project-name-candidates root))))
                             (setf (chat-projects chat)
                                   (append (chat-projects chat)
                                           (list (cons (or name root) root))))
                             (or name root)))))
            (push (cons key name) (chat-workspaces chat))
            name)))))

(defun union-projects (a b)
  (remove-duplicates (append a b) :test #'string= :from-end t))

(defun project-tag (projects)
  "The tag a view line carries, such as \"[maxwell] \", or \"\" for none.
Above three projects it shows the first three and +N."
  (cond
    ((null projects) "")
    ((> (length projects) 3)
     (format nil "[~{~A~^,~},+~D] " (subseq projects 0 3) (- (length projects) 3)))
    (t (format nil "[~{~A~^,~}] " projects))))

;;; The tree

(defun node-name (level index)
  "A node's name, id+n: its first message and how many it covers."
  (format nil "~D+~D" (* index (expt 2 level)) (expt 2 level)))

(defun node-range (level index)
  "The first message NODE(LEVEL, INDEX) covers and the one just after it."
  (let ((width (expt 2 level)))
    (values (* index width) (* (1+ index) width))))

(defun chat-node (chat level index)
  "NODE(LEVEL, INDEX) when it is built, else NIL."
  (let ((levels (chat-levels chat)))
    (when (< level (length levels))
      (let ((nodes (aref levels level)))
        (and (< -1 index (length nodes)) (aref nodes index))))))

(defun node-built-p (chat level index)
  (not (null (chat-node chat level index))))

(defun store-node (chat level index node)
  (let ((levels (chat-levels chat)))
    (loop while (<= (length levels) level)
          do (vector-push-extend (make-growable-vector) levels))
    (let ((nodes (aref levels level)))
      (loop while (<= (length nodes) index)
            do (vector-push-extend nil nodes))
      (setf (aref nodes index) node))))

(defun kind-label (kind)
  (string-downcase (symbol-name kind)))

(defun leaf-source (message)
  "The text a message's line is made from: \"kind: text\"."
  (format nil "~A: ~A" (kind-label (message-kind message)) (message-text message)))

(defun node-projects-for (chat level index)
  (if (zerop level)
      (let ((project (message-project (logged-message chat index))))
        (and project (list project)))
      (union-projects (node-projects (chat-node chat (1- level) (* 2 index)))
                      (node-projects (chat-node chat (1- level) (1+ (* 2 index)))))))

(defun job-pending-p (chat key)
  (or (member key (chat-ready chat) :test #'equal)
      (member key (chat-retry chat) :test #'equal)
      (nth-value 1 (gethash key (chat-jobs chat)))))

(defun enqueue-node (chat level index)
  "Queue NODE(LEVEL, INDEX) for the compactor unless it is built or pending."
  (let ((key (cons level index)))
    (unless (or (node-built-p chat level index) (job-pending-p chat key))
      (setf (chat-ready chat) (append (chat-ready chat) (list key))))))

(defun install-node (chat level index text)
  "Record TEXT as NODE(LEVEL, INDEX), built once and never again. When its
sibling is built too, the parent is joined at once if both fit in one line,
or queued for the compactor."
  (unless (node-built-p chat level index)
    (store-node chat level index
                (make-node :text text :size (octet-length text)
                           :projects (node-projects-for chat level index)))
    (when (zerop level)
      (incf (chat-built-leaves chat))
      (let ((frontier (chat-leaf-frontier chat)))
        (loop while (node-built-p chat 0 frontier) do (incf frontier))
        (setf (chat-leaf-frontier chat) frontier)))
    (let* ((left-index (logandc2 index 1))
           (left (chat-node chat level left-index))
           (right (chat-node chat level (1+ left-index))))
      (when (and left right)
        (if (<= (+ (node-size left) 1 (node-size right)) +node-limit+)
            (install-node chat (1+ level) (floor index 2)
                          (format nil "~A~%~A" (node-text left) (node-text right)))
            (enqueue-node chat (1+ level) (floor index 2)))))))

(defun release-leaves (chat)
  "Make the next messages' leaves buildable while fewer than *LEAF-LOOKAHEAD*
earlier leaves are unbuilt. A short message is its own line at once; a long
one is queued for the compactor."
  (loop while (and (< (chat-next-leaf chat) (chat-message-count chat))
                   (< (- (chat-next-leaf chat) (chat-built-leaves chat))
                      *leaf-lookahead*))
        do (let* ((index (chat-next-leaf chat))
                  (source (leaf-source (logged-message chat index))))
             (incf (chat-next-leaf chat))
             (if (<= (octet-length source) +node-limit+)
                 (install-node chat 0 index source)
                 (enqueue-node chat 0 index)))))

(defun summarized-before-p (chat id)
  "True when every message before ID has its line built, so a turn for
message ID can show the whole chat before it."
  (>= (chat-leaf-frontier chat) id))

;;; Views

(defun line-text (chat level index)
  (let ((node (chat-node chat level index)))
    (format nil "~A|~A~A" (node-name level index) (project-tag (node-projects node))
            (substitute-if #\Space (lambda (character)
                                     (member character '(#\Newline #\Return)))
                           (node-text node)))))

(defun line-size (chat line)
  "Bytes LINE adds to a rendered view, tags and newline included; 0 when unbuilt."
  (let ((node (chat-node chat (car line) (cdr line))))
    (if node
        (+ (length (node-name (car line) (cdr line))) 1
           (octet-length (project-tag (node-projects node)))
           (node-size node) 1)
        0)))

(defun view-size (chat view)
  "Bytes of VIEW's built lines as rendered (the <chat> tags aside)."
  (loop for line across view sum (line-size chat line)))

(defun pair-due (total level index)
  "How long ago the sibling pair starting at NODE(LEVEL, INDEX) ended, in its
own line size: (T - last) / 2^l, where LAST is the pair's last message.
Measuring from the first message instead churns old lines."
  (let ((width (expt 2 level)))
    (/ (- total (1- (* (+ index 2) width))) width)))

(defun most-due-pair (view total mergeable-p)
  "The position in VIEW of the most due sibling pair for which MERGEABLE-P
holds of its parent (LEVEL INDEX), oldest first among equals, or NIL."
  (let ((best nil) (best-due nil))
    (loop for position from 0 below (1- (length view))
          for (level . index) = (aref view position)
          for (next-level . next-index) = (aref view (1+ position))
          do (when (and (= level next-level) (evenp index) (= next-index (1+ index))
                        (funcall mergeable-p (1+ level) (floor index 2)))
               (let ((due (pair-due total level index)))
                 (when (or (null best-due) (> due best-due))
                   (setf best position best-due due)))))
    best))

(defun merge-view-pair (view position)
  "Replace the pair at POSITION in VIEW with its parent line."
  (destructuring-bind (level . index) (aref view position)
    (setf (aref view position) (cons (1+ level) (floor index 2)))
    (replace view view :start1 (1+ position) :start2 (+ position 2))
    (decf (fill-pointer view))
    view))

(defun merge-view (chat view target)
  "Merge VIEW's most due pairs whose parent is built until its size is at most
TARGET. Returns true when it got there, and the number of merges made."
  (let ((size (view-size chat view))
        (total (chat-message-count chat))
        (merges 0)
        (mergeable (lambda (level index) (node-built-p chat level index))))
    (loop while (> size target)
          do (let ((position (most-due-pair view total mergeable)))
               (unless position (return))
               (let ((left (aref view position))
                     (right (aref view (1+ position))))
                 (merge-view-pair view position)
                 (incf merges)
                 (incf size (- (line-size chat (aref view position))
                               (line-size chat left) (line-size chat right))))))
    (values (<= size target) merges)))

(defun update-views (chat)
  "Apply the sawtooth after a message was appended. The view only grows at
its end, until it passes the high-water mark and one batch merges it down."
  (let ((batched nil))
    (when (or (chat-view-merging chat)
              (> (view-size chat (chat-view chat)) *view-high-water*))
      (multiple-value-bind (reached merges)
          (merge-view chat (chat-view chat) *view-low-water*)
        (setf (chat-view-merging chat) (not reached)
              batched (plusp merges))))
    ;; The compaction view is the chat's view merged further: restart it from
    ;; the chat's view whenever that merges, so the two keep the same cuts.
    (when batched
      (let ((copy (make-growable-vector)))
        (loop for line across (chat-view chat) do (vector-push-extend line copy))
        (setf (chat-compaction-view chat) copy
              (chat-compaction-view-merging chat) t)))
    (when (or (chat-compaction-view-merging chat)
              (> (view-size chat (chat-compaction-view chat))
                 *compaction-view-high-water*))
      (setf (chat-compaction-view-merging chat)
            (not (merge-view chat (chat-compaction-view chat)
                             *compaction-view-low-water*))))))

(defun render-lines (chat lines)
  (with-output-to-string (stream)
    (format stream "<chat>~%")
    (dolist (line lines)
      (write-line (line-text chat (car line) (cdr line)) stream))
    (write-string "</chat>" stream)))

(defun built-view-lines (chat view &optional end)
  "VIEW's lines up to the first unbuilt one, and, given END, only those
covering messages before END."
  (loop for line across view
        while (and (node-built-p chat (car line) (cdr line))
                   (or (null end)
                       (<= (nth-value 1 (node-range (car line) (cdr line))) end)))
        collect line))

(defun render-view (chat &key end)
  "The view as the model sees it: <chat>, one id+n|[project] text line per
node, and </chat>. Lines not summarized yet are left out, as are lines
reaching message END or later when END is given."
  (render-lines chat (built-view-lines chat (chat-view chat) end)))

(defun view-ready-through-p (chat id)
  "True when the view shows every message before ID. A turn for message ID
waits for this before it calls the model."
  (summarized-before-p chat id))

;;; The log

(defun clip-tool-output (text)
  "TEXT clipped to its head and tail, at most *MESSAGE-LIMIT* characters in all."
  (if (<= (length text) *message-limit*)
      text
      (let* ((marker (format nil "~%[... ~D characters clipped ...]~%"
                             (- (length text) *message-limit*)))
             (keep (- *message-limit* (length marker)))
             (head (ceiling keep 2))
             (tail (floor keep 2)))
        (concatenate 'string (subseq text 0 head) marker
                     (subseq text (- (length text) tail))))))

(defun split-text (text)
  "TEXT in consecutive pieces of at most *MESSAGE-LIMIT* characters."
  (if (<= (length text) *message-limit*)
      (list text)
      (loop for start from 0 below (length text) by *message-limit*
            collect (subseq text start (min (length text) (+ start *message-limit*))))))

(defun chat-append (chat kind text &key workspace (time (get-universal-time)))
  "Log TEXT as a KIND message from WORKSPACE and return the new ids.
Messages are only appended. A tool result (:ECHO) is clipped to its head and
tail, 30,000 characters in all; any other long text becomes several messages
in a row. Each message appends its line to the view, and the compactor's
failed jobs are requeued."
  (unless (member kind *message-kinds*)
    (error "Unknown message kind ~S." kind))
  (let ((project (workspace-project chat workspace))
        (workspace (and workspace (directory-namestring* workspace)))
        (ids '()))
    (dolist (piece (if (eq kind :echo) (list (clip-tool-output text)) (split-text text)))
      (let ((id (chat-message-count chat)))
        (vector-push-extend (make-message :id id :kind kind :text piece
                                          :size (octet-length piece) :time time
                                          :workspace workspace :project project)
                            (chat-messages chat))
        (vector-push-extend (cons 0 id) (chat-view chat))
        (vector-push-extend (cons 0 id) (chat-compaction-view chat))
        (release-leaves chat)
        (update-views chat)
        (push id ids)))
    ;; A failed compaction is tried again after the next message.
    (setf (chat-ready chat) (append (chat-ready chat) (chat-retry chat))
          (chat-retry chat) '())
    (nreverse ids)))

;;; Model-facing functions

(defun require-chat ()
  (or *chat* (error "There is no chat to read.")))

(defun power-of-two-p (n)
  (and (integerp n) (plusp n) (zerop (logand n (1- n)))))

(defun zoom (id n &optional (page 1))
  "Open the line ID+N of the view into the two lines of N/2 under it; N = 1
gives message ID whole, in pages of *ZOOM-PAGE-SIZE* characters when long
(PAGE selects one). N must be a power of two and ID a multiple of N."
  (let ((chat (require-chat)))
    (unless (power-of-two-p n)
      (error "zoom: n must be a power of two (1, 2, 4, ...), not ~S." n))
    (unless (and (integerp id) (>= id 0) (zerop (mod id n)))
      (error "zoom: id must be a non-negative multiple of n = ~D, not ~S." n id))
    (unless (< id (chat-message-count chat))
      (error "zoom: there is no message ~D; the chat has ~D." id
             (chat-message-count chat)))
    (if (= n 1)
        (let* ((message (logged-message chat id))
               (text (leaf-source message))
               (pages (max 1 (ceiling (length text) *zoom-page-size*))))
          (unless (and (integerp page) (<= 1 page pages))
            (error "zoom: message ~D has ~D page~:P; page ~S does not exist."
                   id pages page))
          (let ((body (subseq text (* (1- page) *zoom-page-size*)
                              (min (length text) (* page *zoom-page-size*))))
                (head (format nil "~D+1|~A" id
                              (project-tag (and (message-project message)
                                                (list (message-project message)))))))
            (if (= pages 1)
                (concatenate 'string head body)
                (format nil "~A[page ~D of ~D~A]~%~A" head page pages
                        (if (< page pages)
                            (format nil "; next: (maxwell:zoom ~D 1 ~D)" id (1+ page))
                            "")
                        body))))
        (let ((level (1- (integer-length n)))
              (index (/ id n)))
          (format nil "~{~A~^~%~}"
                  (loop for child in (list (* 2 index) (1+ (* 2 index)))
                        for (start) = (multiple-value-list (node-range (1- level) child))
                        when (< start (chat-message-count chat))
                          collect (if (node-built-p chat (1- level) child)
                                      (line-text chat (1- level) child)
                                      (format nil "~A|(not summarized yet: zoom it)"
                                              (node-name (1- level) child)))))))))

(defun iso-8601 (universal-time)
  (multiple-value-bind (second minute hour day month year)
      (decode-universal-time universal-time 0)
    (format nil "~4,'0D-~2,'0D-~2,'0DT~2,'0D:~2,'0D:~2,'0DZ"
            year month day hour minute second)))

(defun date (id)
  "The date and time of message ID, as ISO-8601 in UTC."
  (let ((chat (require-chat)))
    (unless (and (integerp id) (>= id 0))
      (error "date: id must be a non-negative integer, not ~S." id))
    (let ((message (logged-message chat id)))
      (unless message
        (error "date: there is no message ~D; the chat has ~D." id
               (chat-message-count chat)))
      (iso-8601 (message-time message)))))

;;; Imports

(defun sha256-file (path)
  "The SHA-256 of PATH's bytes as lowercase hex, computed by openssl."
  (multiple-value-bind (output stderr status)
      (uiop:run-program (list "openssl" "dgst" "-sha256" "-r" (namestring path))
                        :output :string :error-output :string
                        :ignore-error-status t)
    (let ((hex (subseq output 0 (min 64 (length output)))))
      (unless (and (zerop status) (= (length hex) 64)
                   (every (lambda (character) (digit-char-p character 16)) hex))
        (error "Could not hash ~A: ~A" path stderr))
      (string-downcase hex))))

(defun import-file (chat path workspace)
  "Log the file at PATH as :NOTE messages tagged with WORKSPACE's project,
starting \"[NAME PATH]\", unless the same contents were already imported for
that project. Returns the new message ids, or NIL when nothing changed."
  (let* ((path (truename path))
         (project (workspace-project chat workspace))
         (key (cons project (sha256-file path))))
    (unless (member key (chat-imports chat) :test #'equal)
      (prog1 (chat-append chat :note
                          (format nil "[~A ~A]~%~A" (file-namestring path) (namestring path)
                                  (uiop:read-file-string path :external-format :utf-8))
                          :workspace workspace)
        (setf (chat-imports chat) (append (chat-imports chat) (list key)))))))

(defun import-agents-md (chat workspace)
  "Import AGENTS.md from WORKSPACE's git root (or WORKSPACE) once per version.
An edited file is imported again, so its latest version is the newest thing
the view says about it. Returns the new message ids, or NIL."
  (let ((path (probe-file (merge-pathnames "AGENTS.md" (project-root workspace)))))
    (when (and path (uiop:file-exists-p path))
      (import-file chat path workspace))))

;;; Migration and reporting

(defun tool-call-text (call)
  "The text logged for a COMMON_LISP tool CALL: its code, or else its name
and raw arguments."
  (let* ((function (get-key call "function"))
         (name (and (hash-table-p function) (get-key function "name")))
         (arguments (and (hash-table-p function) (get-key function "arguments" "")))
         (decoded (and (stringp arguments)
                       (ignore-errors (decode arguments))))
         (code (and (hash-table-p decoded) (get-key decoded "code"))))
    (if (stringp code)
        code
        (format nil "~A ~A" (or name "tool")
                (if (stringp arguments) arguments (encode arguments))))))

(defun migrate-legacy-history (chat history &key workspace)
  "Log the messages of an old linear agent HISTORY (OpenAI-style JSON objects)
as :USER, :MAXWELL, :TOOL, and :ECHO messages. The caller clears the history.
Returns the number of messages logged."
  (let ((count 0))
    (flet ((log-text (kind text)
             (when (and (stringp text) (plusp (length text)))
               (incf count (length (chat-append chat kind text :workspace workspace))))))
      (dolist (message history count)
        (let ((role (get-key message "role" "")))
          (cond
            ((string= role "user") (log-text :user (get-key message "content")))
            ((string= role "assistant")
             (log-text :maxwell (get-key message "content"))
             (let ((calls (get-key message "tool_calls")))
               (when (vectorp calls)
                 (loop for call across calls do (log-text :tool (tool-call-text call))))))
            ((string= role "tool") (log-text :echo (get-key message "content")))))))))

(defun built-node-count (chat)
  (loop for nodes across (chat-levels chat)
        sum (count-if-not #'null nodes)))

(defun complete-node-count (chat)
  "How many nodes cover a range the log already holds in full."
  (loop with total = (chat-message-count chat)
        for width = 1 then (* 2 width)
        while (<= width total)
        sum (floor total width)))

(defun memory-report (chat)
  "A short description of CHAT for /memory: messages, nodes, views, and jobs."
  (let ((built (built-node-count chat)))
    (format nil "~D message~:P; ~D node~:P built, ~D unbuilt~%~
                 view: ~D line~:P, ~D bytes (~D line~:P shown)~%~
                 compaction view: ~D line~:P, ~D bytes~%~
                 compactor: ~D job~:P in flight, ~D ready, ~D to retry"
            (chat-message-count chat) built
            (max 0 (- (complete-node-count chat) built))
            (length (chat-view chat)) (view-size chat (chat-view chat))
            (length (built-view-lines chat (chat-view chat)))
            (length (chat-compaction-view chat))
            (view-size chat (chat-compaction-view chat))
            (hash-table-count (chat-jobs chat))
            (length (chat-ready chat)) (length (chat-retry chat)))))
