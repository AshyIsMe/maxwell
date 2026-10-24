(in-package #:maxwell)

(defun usage (&optional (stream *standard-output*))
  (write-string "Maxwell — a small Common Lisp coding agent

Usage: maxwell [options] [prompt ...]

  -m, --model MODEL       model name (or MAXWELL_MODEL)
      --compact-model M   cheaper model for memory summaries
                            (or MAXWELL_COMPACT_MODEL; default: --model)
  -b, --base-url URL      OpenAI-compatible /v1 URL
  -e, --endpoint URL      exact API endpoint URL
  -w, --wire-api API      chat (default) or responses
  -k, --api-key-env NAME  environment variable holding the API key
  -a, --auth PROVIDER     use a saved login: openai or safeai
      --login PROVIDER    sign in, save credentials, then exit:
                            openai  ChatGPT plan (OAuth)
                            safeai  Safe AI Labs (API key via browser)
      --logout PROVIDER   forget a saved login, then exit
  -H, --header HEADER     add a provider HTTP header (repeatable)
      --request-json JSON add provider-specific request fields
  -C, --workspace DIR     project directory
  -p, --print PROMPT      run once and exit
      --color WHEN        auto (default), always, or never
      --fresh             run this binary or image itself instead of the
                            global image (its chat replaces the global one
                            when it saves)
      --ephemeral         a throwaway chat that is never saved
      --adopt-chat        move the global image's chat into this build
      --export-chat       print this image's chat as s-expressions
  -h, --help              show this help

Environment: OPENAI_API_KEY, MAXWELL_MODEL, MAXWELL_COMPACT_MODEL,
             MAXWELL_BASE_URL, MAXWELL_ENDPOINT, MAXWELL_WIRE_API,
             MAXWELL_API_KEY_ENV, MAXWELL_SESSION_FILE, MAXWELL_IMAGE,
             MAXWELL_OAUTH_MODE (auto, manual, loopback).
  openai:    MAXWELL_AUTH_FILE, MAXWELL_CODEX_MODEL (default gpt-5.6-sol).
  safeai:    SAFEAILABS_API_KEY (overrides the saved key),
             MAXWELL_SAFEAI_AUTH_FILE, MAXWELL_SAFEAI_BASE_URL,
             MAXWELL_SAFEAI_MODEL.

The chat lives in one global image, MAXWELL_IMAGE or
~/.local/share/maxwell/maxwell.image. Without a provider option, Maxwell
reuses the provider and model of the previous session, remembered in
~/.config/maxwell/session.json.
" stream))

(defun take-option (arguments option)
  (unless arguments (error "~A needs a value." option))
  (values (first arguments) (rest arguments)))

(defparameter *value-options*
  '((:model "-m" "--model") (:compact-model "--compact-model")
    (:base-url "-b" "--base-url") (:endpoint "-e" "--endpoint")
    (:wire-api "-w" "--wire-api") (:api-key-env "-k" "--api-key-env")
    (:auth-provider "-a" "--auth") (:login-provider "--login")
    (:logout-provider "--logout") (:header "-H" "--header")
    (:request-json "--request-json") (:workspace "-C" "--workspace")
    (:prompt "-p" "--print") (:color "--color")
     (:resume-state "--resume-state") (:verify-resume "--verify-resume"))
  "Options that take a value: the key PARSE-OPTIONS returns it under, then
the option's spellings.")

(defparameter *flag-options*
  '((:help "-h" "--help") (:fresh "--fresh") (:ephemeral "--ephemeral")
    (:adopt-chat "--adopt-chat") (:export-chat "--export-chat"))
  "Options without a value, in the same form as *VALUE-OPTIONS*.")

(defun parse-options (arguments)
  "ARGUMENTS as a plist of the keys in *VALUE-OPTIONS* and *FLAG-OPTIONS*.
:HEADERS lists every --header in order. The first argument that is not an
option starts the prompt, which takes the rest."
  (let ((options '()) (headers '()))
    (loop while arguments
          for argument = (pop arguments)
          for value-option = (find argument *value-options* :key #'rest
                                                            :test (lambda (a names) (member a names :test #'string=)))
          for flag-option = (find argument *flag-options* :key #'rest
                                                          :test (lambda (a names) (member a names :test #'string=)))
          do (cond
               (flag-option (setf (getf options (first flag-option)) t))
               ((eq (first value-option) :header)
                (multiple-value-bind (header rest) (take-option arguments argument)
                  (push header headers)
                  (setf arguments rest)))
               (value-option
                (multiple-value-bind (value rest) (take-option arguments argument)
                  (setf (getf options (first value-option)) value
                        arguments rest)))
               ((and (plusp (length argument)) (char= (char argument 0) #\-))
                (error "Unknown option ~A." argument))
               (t
                (setf (getf options :prompt) (format nil "~{~A~^ ~}" (cons argument arguments))
                      arguments nil))))
    (list* :headers (nreverse headers) options)))

(defun command-p (line name)
  (string= (string-trim '(#\Space #\Tab) line) name))

(defun command-argument (line prefix)
  (when (and (>= (length line) (length prefix))
             (string= line prefix :end1 (length prefix) :end2 (length prefix)))
    (string-trim '(#\Space #\Tab) (subseq line (length prefix)))))

(defun provider-options-given-p (base-url endpoint wire-api api-key-env auth-provider
                                 headers lookup)
  "True when a provider was named explicitly, on the command line or in the
environment. Naming a provider starts a fresh configuration instead of reusing
the remembered session."
  (or base-url endpoint wire-api api-key-env auth-provider headers
      (funcall lookup "MAXWELL_BASE_URL")
      (funcall lookup "MAXWELL_ENDPOINT")
      (funcall lookup "MAXWELL_WIRE_API")
      (funcall lookup "MAXWELL_API_KEY_ENV")))

(defparameter *login-providers*
  '(("openai" :openai "sign in with ChatGPT (OAuth)")
    ("safeai" :safeai "save a Safe AI Labs API key"))
  "The providers --login, --auth, and --logout accept: name, keyword, summary.")

(defparameter +default-codex-model+ "gpt-5.6-sol")

(defun login-provider-keyword (name)
  "The keyword for login provider NAME, or an error naming the supported ones."
  (or (second (find name *login-providers* :key #'first :test #'string-equal))
      (error "Unknown provider ~S; supported providers: ~{~A~^, ~}."
             name (mapcar #'first *login-providers*))))

(defun auth-provider-settings (auth-provider lookup)
  "MAKE-CLIENT settings implied by AUTH-PROVIDER, as two plists. The first holds
those set by the provider's own environment variables, which outrank generic
MAXWELL_* variables and the remembered session; the second holds defaults that
fill only what nothing else set."
  (flet ((setting (name variable)
           (let ((value (funcall lookup variable)))
             (and value (list name value)))))
    (case auth-provider
      (:openai
       ;; MAKE-CLIENT itself forces the Responses endpoint and request contract.
       (values (setting :model "MAXWELL_CODEX_MODEL")
               (list :model +default-codex-model+)))
      (:safeai
       (let ((record (ignore-errors (load-safeai-auth-record))))
         (values (append (setting :model "MAXWELL_SAFEAI_MODEL")
                         (setting :base-url "MAXWELL_SAFEAI_BASE_URL"))
                 (list :model (or (session-string record "model") "")
                       :base-url (or (session-string record "base_url")
                                     +safeai-base-url+)
                       :wire-api :chat-completions
                       :api-key-environment +safeai-api-key-environment+)))))))

(defun resolve-client-options (&key model compact-model base-url endpoint wire-api
                                    api-key-env auth-provider headers
                                    (environment #'environment-value))
  "Merge the command line, the environment, and the previous session's provider
and model into MAKE-CLIENT arguments. Explicit settings always win; the
remembered session is used only when the caller named no provider at all.
ENVIRONMENT names the lookup for MAXWELL_* variables, which tests replace."
  (flet ((lookup (name) (funcall environment name)))
    (let* ((remembered
             (unless (provider-options-given-p base-url endpoint wire-api api-key-env
                                               auth-provider headers #'lookup)
               (remembered-client-options (load-session-record))))
           (auth-provider (if auth-provider
                              (login-provider-keyword auth-provider)
                              (getf remembered :auth-provider))))
      (multiple-value-bind (preferred defaults)
          (auth-provider-settings auth-provider #'lookup)
        (list :model (or model (getf preferred :model) (lookup "MAXWELL_MODEL")
                         (getf remembered :model) (getf defaults :model) "")
              :compact-model (or compact-model (lookup "MAXWELL_COMPACT_MODEL")
                                 (getf remembered :compact-model))
              :base-url (or base-url (getf preferred :base-url)
                            (lookup "MAXWELL_BASE_URL") (getf remembered :base-url)
                            (getf defaults :base-url) "https://api.openai.com/v1")
              :endpoint (or endpoint (lookup "MAXWELL_ENDPOINT")
                            (getf remembered :endpoint))
              :wire-api (or wire-api (lookup "MAXWELL_WIRE_API")
                            (getf remembered :wire-api) (getf defaults :wire-api)
                            :chat-completions)
              :api-key-environment (or api-key-env (lookup "MAXWELL_API_KEY_ENV")
                                       (getf remembered :api-key-environment)
                                       (getf defaults :api-key-environment)
                                       "OPENAI_API_KEY")
              :auth-provider auth-provider
              :extra-headers (or headers (getf remembered :extra-headers))
              :effort (getf remembered :effort))))))

(defun adopt-openai-client (client)
  "Point CLIENT at the ChatGPT-plan Responses endpoint with a Codex model, and
remember it as the provider of this session."
  (configure-client-for-openai-oauth client)
  (setf (client-model client)
        (or (environment-value "MAXWELL_CODEX_MODEL") +default-codex-model+))
  (remember-client client)
  client)

(defun adopt-safeai-client (client model)
  "Point CLIENT at Safe AI Labs Chat Completions with MODEL, and remember it as
the provider of this session."
  (configure-client-for-safeai client model)
  (remember-client client)
  client)

(defun login-and-adopt (name client)
  "Sign in to login provider NAME and switch CLIENT to it."
  (ecase (login-provider-keyword name)
    (:openai (openai-login) (adopt-openai-client client))
    (:safeai (adopt-safeai-client client (safeai-login)))))

(defun logout (name)
  "Forget the saved login for provider NAME and say so."
  (let ((forgotten (ecase (login-provider-keyword name)
                     (:openai (forget-openai-login))
                     (:safeai (forget-safeai-login)))))
    (format t "~:[No saved ~A login to forget.~;Forgot the saved ~A login.~]~%"
            forgotten (string-downcase name))))

(defun login-usage ()
  (format nil "Usage: /login PROVIDER or /logout PROVIDER, where PROVIDER is one of:~:{~%  ~A~16T~*~A~}"
          *login-providers*))

(defun configure-client-for-safeai (client model)
  "Point CLIENT at Safe AI Labs Chat Completions with MODEL, keeping the
session's thinking effort."
  (let ((effort (client-effort client)))
    (setf (client-base-url client) (trim-slashes (safeai-base-url))
          (client-endpoint client) nil
          (client-wire-api client) :chat-completions
          (client-auth-provider client) :safeai
          (client-api-key-environment client) +safeai-api-key-environment+
          ;; Headers and request fields belonged to the previous provider; the
          ;; ChatGPT plan's stream:true in particular would break Chat
          ;; Completions. The thinking effort belongs to the session, so it
          ;; stays, and is sent in the new provider's spelling.
          (client-extra-headers client) '()
          (client-request-fields client) nil
          (client-model client) model)
    (when effort (set-client-effort client effort)))
  client)

(defun provider-display-name (keyword)
  "The name /model and /login print for provider KEYWORD."
  (or (first (find keyword *login-providers* :key #'second))
      (if keyword (string-downcase (symbol-name keyword)) "this session")))

(defvar *model-credential-environment* #'environment-value
  "The lookup /model uses to see whether a provider's key is in the environment;
tests replace it.")

(defun login-provider-saved-p
    (keyword &optional (environment #'environment-value))
  "True when /model may use login provider KEYWORD: its credential is saved."
  (case keyword
    (:openai (openai-login-saved-p))
    (:safeai (safeai-login-saved-p environment))
    (t nil)))

(defun login-provider-client (keyword base)
  "A copy of BASE pointed at login provider KEYWORD, with no model chosen yet.
The compact model is dropped, because it belongs to the provider it was set for."
  (let ((client
          (ecase keyword
            (:openai
             (let ((client (configure-client-for-openai-oauth (copy-client base))))
               (setf (client-model client) "")
               client))
            (:safeai (configure-client-for-safeai (copy-client base) "")))))
    (setf (client-compact-model client) nil)
    client))

(defun provider-clients (agent)
  "The (KEYWORD . CLIENT) pairs /model works with: every signed-in provider,
with the session's own provider first. The session's provider is listed even
when its credential comes from the environment rather than a saved login."
  (let* ((current (agent-client agent))
         (current-keyword (client-auth-provider current))
         (pairs (list (cons current-keyword current))))
    (dolist (entry *login-providers* (nreverse pairs))
      (let ((keyword (second entry)))
        (unless (eq keyword current-keyword)
          (when (login-provider-saved-p keyword *model-credential-environment*)
            (push (cons keyword (login-provider-client keyword current))
                  pairs)))))))

(defvar *model-lister* #'list-provider-models
  "The function /model calls for a provider's model catalog; tests replace it.")

(defun provider-model-catalog (client keyword)
  "CLIENT's model catalog, or an error naming the provider KEYWORD."
  (handler-case (funcall *model-lister* client)
    (error (condition)
      (error "Could not list ~A's models: ~A" (provider-display-name keyword)
             condition))))

(defun print-model-catalog (agent)
  "Show every provider this installation is signed in to, with its models and
the one this session uses."
  (ui-call #'status-bar-hide)
  (let* ((current (agent-client agent))
         (keyword (client-auth-provider current)))
    (format t "~&Current model: ~A~@[ from ~A~]~%"
            (client-model current)
            (and keyword (provider-display-name keyword)))
    (dolist (pair (provider-clients agent))
      (let* ((client (cdr pair))
             (here (eq client current)))
        (format t "~&~:[ ~;*~] ~A~8T~A~%"
                here (provider-display-name (car pair))
                (styled :dim (client-base-url client)))
        (handler-case
            (dolist (model (funcall *model-lister* client))
              (format t "~&~A    ~A~@[ ~A~]~%"
                      (if (and here (string= (car model) (client-model current)))
                          ">" " ")
                      (car model)
                      (unless (string= (car model) (cdr model))
                        (styled :dim (format nil "— ~A" (cdr model))))))
          (error (condition)
            (format t "~&     ~A~%"
                    (styled :error (format nil "could not list models: ~A"
                                           condition)))))))
    t))

(defun run-model-command (agent &optional model-id)
  (let ((text (call-with-command-prompt
                agent (lambda () (%run-model-command agent model-id)))))
    (write-string text)))

(defun %run-model-command (agent &optional model-id)
  "Fetch models off the UI thread, pumping queued UI work until completion."
  (status-bar-hide)
  (let ((output nil) (failure nil)
        (done (bt:make-semaphore :count 0))
        (worker nil))
    (unwind-protect
         (progn
           (setf worker
                 (start-worker
                   "maxwell model catalog"
                   (lambda ()
                     (unwind-protect
                          (handler-case
                              (setf output
                                    (with-output-to-string (*standard-output*)
                                      (if model-id (select-model agent model-id)
                                          (print-model-catalog agent))))
                            (error (condition) (setf failure condition)))
                       (bt:signal-semaphore done)))
                   :bindings (list (cons '*ui-thread* *ui-thread*)
                                   (cons '*model-lister* *model-lister*)
                                   (cons '*model-credential-environment* *model-credential-environment*)
                                   (cons '*auth-file* *auth-file*)
                                   (cons '*safeai-auth-file* *safeai-auth-file*)
                                   (cons '*session-file* *session-file*)
                                   (cons '*color* *color*))))
           (loop until (bt:wait-on-semaphore done :timeout 0.01)
                 do (service-ui)
                    (when (take-interrupt) (error "Model lookup interrupted.")))
           (worker-join worker)
           (when failure (error failure))
           output)
      (when (worker-alive-p worker)
        (bt:destroy-thread (worker-thread worker))
        (worker-join worker)))))

(defun select-model (agent model-id)
  "The /model MODEL command: choose MODEL, switching to another signed-in
provider when only it offers the model. A provider's own name chooses that
provider and keeps the current model when the provider lists it."
  (ui-call #'status-bar-hide)
  (when (zerop (length model-id))
    (error "Usage: /model MODEL, or /model PROVIDER"))
  (let* ((pairs (provider-clients agent))
         (current (agent-client agent))
         (current-pair (find current pairs :key #'cdr))
         (named (find model-id pairs
                      :key (lambda (pair) (provider-display-name (car pair)))
                      :test #'string-equal))
         (chosen nil))
    (cond
      (named
       (let* ((models (provider-model-catalog (cdr named) (car named)))
              (model (or (car (find (client-model current) models
                               :key #'car :test #'string=))
                         (caar models))))
         (unless model
           (error "~A lists no models." (provider-display-name (car named))))
         (setf (client-model (cdr named)) model
               chosen named)))
      (t
       ;; Search this session's provider first, then the others.
       (dolist (pair (if current-pair
                         (cons current-pair (remove current-pair pairs))
                         pairs))
         (let ((models (handler-case (funcall *model-lister* (cdr pair))
                         (error () nil))))
           (when (find model-id models :key #'car :test #'string=)
             (setf (client-model (cdr pair)) model-id
                   chosen pair)
             (return))))))
    (unless chosen
      (error "Model ~S is not available from ~{~A~^, ~}." model-id
             (mapcar (lambda (pair) (provider-display-name (car pair))) pairs)))
    (setf (agent-client agent) (cdr chosen))
    (remember-client (agent-client agent))
    (format t "~&~A~%"
            (styled :dim (format nil "Selected ~A from ~A."
                                 (client-model (cdr chosen))
                                 (provider-display-name (car chosen)))))))

;;; Thinking effort

(defparameter *thinking-levels* '("none" "minimal" "low" "medium" "high")
  "The thinking levels /effort accepts. A provider that takes others can be
given one with --request-json.")

(defun set-effort (client level)
  "Set CLIENT's thinking LEVEL, or clear it with default. The choice is
remembered with the model. Returns the level, or NIL for the provider's own
default."
  (let ((level (string-downcase (string-trim '(#\Space #\Tab) level))))
    (cond
      ((member level '("default" "unset" "clear" "-") :test #'string=)
       (set-client-effort client nil))
      ((member level *thinking-levels* :test #'string=)
       (set-client-effort client level))
      (t
       (error "Unknown thinking level ~S. Try ~{~A~^, ~}, or default."
              level *thinking-levels*)))
    (remember-client client)
    (client-effort client)))

(defun effort-command (client argument)
  "The /effort command: say CLIENT's thinking effort, or set it from ARGUMENT."
  (let ((argument (string-trim '(#\Space #\Tab) argument)))
    (if (zerop (length argument))
        (format t "~&Thinking effort: ~A. Use /effort LEVEL, one of ~{~A~^, ~}, ~
                   or /effort default.~%"
                (or (client-effort client) "the provider's default")
                *thinking-levels*)
        (format t "~&Thinking effort: ~A.~%"
                (or (set-effort client argument) "the provider's default")))))

(defun run-interactive-turn (line agent)
  (if (null *conversation*)
      (%run-interactive-turn line agent)
      (let ((origin *selected-conversation*))
        (unwind-protect
             (let ((*conversation* origin)
                   (*ui-context-wrapper* (conversation-ui-wrapper origin)))
               (deliver-conversation-notes origin)
               (when (conversation-running-p origin) (queue-steering line))
               (%run-interactive-turn line agent))
          (select-conversation (conversation-name *selected-conversation*) :draw nil)
          (service-conversations)))))

(defun %run-interactive-turn (line agent)
  "Run LINE on a worker thread, keeping the prompt live on this, the UI,
thread: the user can browse the transcript and queue steering prompts, and
Ctrl-C cancels the turn. The compactor has a worker of its own."
  (set-activity :idle)
  (unwind-protect
       (let ((state (interactive-terminal-state)))
         (if (null state)
             (run line :agent agent)
             (unwind-protect
                  (progn
                    (terminal-command "-icanon" "-echo" "min" "1" "time" "0")
                    (let* ((session (make-steering-session agent *standard-input*
                                                           *standard-output*))
                           ;; The steering line is drawn on the UI thread, and the
                           ;; worker hides it from ITS thread when the output
                           ;; starts, so the hide is handed over.
                           (*input-hide* (lambda () (ui-call (lambda ()
                              (when (conversation-visible-p) (steering-hide session))))))
                           ;; A streamed reasoning trace shows while the model
                           ;; thinks, and the UI thread redraws it.
                           (*reasoning-delta-function* #'note-live-reasoning)
                           (turn (or (and *conversation* (conversation-running-p *conversation*)
                                           (conversation-turn *conversation*))
                                     (start-turn line :agent agent :print-response t
                                                        :bindings
                                                        (list (cons '*input-hide*
                                                                    *input-hide*)
                                                              (cons '*reasoning-delta-function*
                                                                    #'note-live-reasoning))))))
                      (setf (steering-session-turn session) turn)
                      ;; Keystrokes and queued work come to the UI thread here.
                      (let ((*ui-service* (lambda () (service-steering-input session)))
                             (*ui-input-hide* (lambda () (steering-hide session))))
                        (unwind-protect (await-turn turn)
                          (steering-hide session)
                          ;; An unfinished steering line becomes the next prompt.
                          (if *conversation*
                               (setf (conversation-draft *conversation*) (steering-draft session))
                               (setf *prompt-draft* (steering-draft session)))
                          ;; A turn that asked for an image gets it now that its
                          ;; worker has ended: the save cannot run beside threads.
                          (when (pending-image-pending-p) (finish-pending-image))))))
               (terminal-command state))))
    (set-activity :idle)))


;;; Reading the log back

(defun older-log-entries (chat start)
  "A function for *OLDER-ENTRIES-FUNCTION* that pages back through CHAT's log
from message START, where this session began, as transcript entries."
  (lambda (oldest count)
    (let* ((end (min start (or (and oldest (entry-id oldest)) start)))
           (begin (max 0 (- end count))))
      (coerce (loop for id from begin below end
                    collect (log-message-entry (logged-message chat id)))
              'vector))))

(defun recent-log-entries (chat &optional (count 200))
  "The last COUNT messages of CHAT as transcript entries."
  (let ((total (chat-message-count chat)))
    (coerce (loop for id from (max 0 (- total count)) below total
                  collect (log-message-entry (logged-message chat id)))
            'vector)))

;;; The interactive session

(defun status (control &rest arguments)
  "Show a dim status line in the transcript."
  (record-entry :status (apply #'format nil control arguments) :show t))

(defun report-error (control &rest arguments)
  "Print an error message on *ERROR-OUTPUT* in the :error face, with the status
bar out of the way."
  (status-bar-hide)
  (format *error-output* "~&~A~%" (styled :error (apply #'format nil control arguments))))

(defun save-session (&key quiet)
  "Checkpoint the global image when this session has one, saying so unless
QUIET. True unless saving failed."
  (or (null *checkpoint-path*)
      (handler-case
          (progn (checkpoint)
                 (unless quiet (status "Chat saved to ~A." (native *checkpoint-path*)))
                 t)
        (error (condition)
          (report-error "Could not save the chat: ~A" condition)
          nil))))

(defparameter *help-text*
  "/login openai  sign in with ChatGPT OAuth
/login safeai  save a Safe AI Labs API key
/logout NAME   forget a saved login
/model         list signed-in providers, their models, and the current one
/model MODEL   select a model, switching provider when only it offers it
/model PROVIDER  switch to a signed-in provider (openai, safeai)
/effort        show the model's thinking effort
/effort LEVEL  set it: none, minimal, low, medium, or high
/statusbar     show or hide the status bar above the prompt
/memory        show the chat's size, summaries, and compactor jobs
/stats         show recent request timings and a tokens-weighted summary
/import PATH   read a file into memory as a note
/checkpoint    save the chat to the global image and keep going
/chat          list conversations; /chat NAME selects one
/chat new NAME fresh child; /chat fork NAME snapshots the selected context
/chat run NAME TASK delegate; /chat note NAME TEXT leaves a non-waking note
/chat ask NAME TEXT requests work; /chat stop NAME cancels work
/restart       build, test, verify saved conversations, then restart with rollback
/reload        live-load compatible source changes, then save
/save PATH     save a separate resumable image and exit
/quit          save the chat and exit (Ctrl-D too)
Editing: Up/Down history; Ctrl-A/E line ends; Ctrl-U/W/K kill; Ctrl-Y yank.
Ctrl-T expands or collapses tool calls and thinking traces; PgUp/PgDn
browse the transcript.
While the model works: type a line and press Enter to steer the model
before its next request.")

(defun import-command (path)
  (let* ((path (workspace-path path))
         (ids (import-file *chat* path (agent-workspace *agent*))))
    (if ids
        (status "Imported ~A as message~P ~D~@[-~D~]." (native path) (length ids)
                (first ids) (and (rest ids) (car (last ids))))
        (status "~A is already in memory, unchanged." (native path)))))

(defun reload-command ()
  (let ((old-prompt *base-system-prompt*))
    (status "Reloaded Maxwell from ~A." (native (reload-maxwell)))
    ;; A session still using the old built-in prompt takes the new one.
    (when (eq (agent-system-prompt *agent*) old-prompt)
      (setf (agent-system-prompt *agent*) *base-system-prompt*))
    (save-session)))

(defun compactor-tick-function (agent &optional (chat *chat*))
  "The function the compactor's worker calls: it advances AGENT's background
compaction in CHAT on the worker, which is a thread of its own. CHAT is taken
now, because a worker inherits no dynamic bindings."
  (lambda ()
    (if *conversations*
        (dolist (conversation (copy-list *conversations*))
          (tick-compactor (conversation-agent conversation) (conversation-chat conversation)))
        (tick-compactor agent chat))))

(defun start-background-work (agent)
  "Start what runs beside the prompt: the compactor's worker and the Ctrl-C
handler. The UI thread has already been claimed by MAIN."
  (install-interrupt-handler)
  (start-compactor-worker (compactor-tick-function agent)))

(defun stop-background-work ()
  (stop-compactor-worker)
  (quiesce-workers))

(defun interactive-loop (&optional (agent *agent*))
  (setf *agent* (or agent (make-agent)))
  (unless *chat* (setf *chat* (make-chat)))
  (ensure-conversations *agent*)
  (start-background-work *agent*)
  (let ((start (with-chat-lock (chat-message-count *chat*))))
    (setf *older-entries-function* (older-log-entries *chat* start))
    ;; A resumed chat shows where it left off.
    (when (plusp start)
      (show-recent-exchanges (recent-log-entries *chat*) :count 2)))
  (record-entry :status
                (format nil "Maxwell ~A — ~A~%Type /help for commands; PgUp/PgDn browse the transcript, Ctrl-T expands tool calls and thoughts, Up/Down recall previous prompts, Ctrl-C stops a turn."
                        (client-model (agent-client *agent*))
                        (namestring (agent-workspace *agent*)))
                :show t)
  (let ((warning (and (plusp (length (client-model (agent-client *agent*))))
                      (compact-model-warning (agent-client *agent*)))))
    (when warning (status "~A" warning)))
  (loop
    (let ((line (read-interactive-prompt *agent*)))
      (cond
        ((or (null line) (command-p line "/quit") (command-p line "/exit"))
         (return (if (save-session) 0 1)))
        ((or (command-p line "/chat") (command-argument line "/chat "))
         (handler-case (conversation-command (or (command-argument line "/chat ") ""))
           (error (e) (report-error "~A" e))))
        ((command-p line "/restart")
         (handler-case (restart-command)
           (error (e) (report-error "Restart failed; current session retained: ~A" e))))
        ((command-p line "/help")
         (write-line (style-lines :dim *help-text*)))
        ((or (command-p line "/login") (command-p line "/logout"))
         (format t "~A~%" (login-usage)))
        ((command-argument line "/login ")
         (handler-case
             (let ((client (login-and-adopt (command-argument line "/login ")
                                            (agent-client *agent*))))
               (format t "The current Maxwell session now uses ~A with ~A.~%"
                       (client-base-url client) (client-model client)))
           (error (condition)
             (report-error "Login failed: ~A" condition))))
        ((command-argument line "/logout ")
         (handler-case (logout (command-argument line "/logout "))
           (error (condition)
             (report-error "Logout failed: ~A" condition))))
        ((command-p line "/model")
         (handler-case (run-model-command *agent*)
           (error (condition)
             (report-error "Could not list models: ~A" condition))))
        ((command-argument line "/model ")
         (handler-case
             (run-model-command *agent* (command-argument line "/model "))
           (error (condition)
             (report-error "Could not select model: ~A" condition))))
        ((or (command-p line "/statusbar")
             (command-argument line "/statusbar "))
         (status-bar-command line (session-client)))
        ((or (command-p line "/effort") (command-argument line "/effort "))
         (handler-case
             (effort-command (agent-client *agent*)
                             (or (command-argument line "/effort ") ""))
           (error (condition)
             (report-error "Could not set the thinking effort: ~A" condition))))
        ((command-p line "/memory")
         (status "~A~%image: ~A" (memory-report *chat*)
                 (if *checkpoint-path* (native *checkpoint-path*) "none (ephemeral)")))
        ((command-p line "/stats")
         (status "~A" (request-stats-report)))
        ((command-argument line "/import ")
         (let ((path (command-argument line "/import ")))
           (if (zerop (length path))
               (format t "Usage: /import PATH~%")
               (handler-case (import-command path)
                 (error (condition)
                   (report-error "Could not import ~A: ~A" path condition))))))
        ((command-p line "/checkpoint")
         (if *checkpoint-path*
             (save-session)
             (status "This session is ephemeral; it is never saved.")))
        ((command-p line "/reload")
         (handler-case (reload-command)
           (error (condition)
             (report-error "Reload failed: ~A" condition))))
        ((command-argument line "/save ")
         (let ((path (command-argument line "/save ")))
           (if (zerop (length path))
               (format t "Usage: /save PATH~%")
               ;; The snapshot ends the process: save the global chat first.
               (when (save-session)
                 (handler-case (save-image (namestring (workspace-path path)))
                   (error (condition)
                     (report-error "Could not save image: ~A" condition)))))))
        ((zerop (length (string-trim '(#\Space #\Tab) line))))
        (t
         (handler-case (run-interactive-turn line *agent*)
           (error (condition)
             (record-entry :status (format nil "Error: ~A" condition) :ok-p nil :show t))
           ;; Ctrl-C abandons the turn; what it said so far stays logged.
           (sb-sys:interactive-interrupt ()
             (record-entry :status "Interrupted." :ok-p nil :show t))))))))

;;; Starting up

(defun open-global-chat (options arguments)
  "Settle which chat this process uses. Unless told otherwise, a process that
is not the global image becomes it, by exec'ing it with the same ARGUMENTS;
the global image's process takes the lock and saves itself when it ends.
--ephemeral uses a throwaway chat and never touches the global image."
  (let ((image (global-image-path)))
    ;; A descriptor or path saved in an image belongs to the process that saved it.
    (setf *lock-fd* nil
          *checkpoint-path* nil)
    (cond
      ((getf options :ephemeral)
       (setf *chat* (make-chat) *conversations* nil *selected-conversation* nil *conversation* nil))
      (t
       (unless (or (getf options :fresh) (getf options :adopt-chat)
                   (running-global-image-p image))
         (when (probe-file image)
           (exec-image image arguments)))
       (unless (acquire-chat-lock (image-lock-path image))
         (error "Another Maxwell is using the global chat in ~A. Quit it first, or run ~
                 maxwell --ephemeral for a separate, throwaway chat."
                (native image)))
       (when (and (getf options :fresh) (probe-file image)
                  (not (running-global-image-p image)))
         (status "--fresh: this session's chat replaces the one in ~A when it saves."
                 (native image)))
       (setf *checkpoint-path* image)))
    (unless *chat* (setf *chat* (make-chat)))
    ;; Jobs saved by mistake refer to processes that are gone.
    (compactor-reset *chat*)
    ;; An image from before the endless chat kept one linear conversation.
    (when (and *agent* (agent-history *agent*))
      (let ((count (migrate-legacy-history *chat* (agent-history *agent*)
                                           :workspace (agent-workspace *agent*))))
        (setf (agent-history *agent*) '())
        (status "Moved ~D message~:P of the old conversation into the chat." count)))))

(defun adopt-global-chat ()
  "--adopt-chat: take the global image's chat into this build and save this
build, with it, as the new global image."
  (let ((image *checkpoint-path*))
    (unless (probe-file image)
      (error "There is no global image at ~A to adopt a chat from." (native image)))
    (setf *chat* (adopt-chat image))
    (checkpoint)
    (format t "Adopted ~D message~:P from ~A; it now runs this build.~%"
            (chat-message-count *chat*) (native image))
    0))

(defun make-session-agent (options)
  "The agent for this session, from OPTIONS, the environment, and the
remembered session."
  (let ((request-fields
          (let ((json (getf options :request-json)))
            (and json
                 (handler-case (decode json)
                   (error (condition)
                     (error "Invalid --request-json: ~A" condition)))))))
    (when (and request-fields (not (hash-table-p request-fields)))
      (error "--request-json must be a JSON object."))
    (let ((client (apply #'make-client
                         (append
                           (resolve-client-options
                             :model (getf options :model)
                             :compact-model (getf options :compact-model)
                             :base-url (getf options :base-url)
                             :endpoint (getf options :endpoint)
                             :wire-api (getf options :wire-api)
                             :api-key-env (getf options :api-key-env)
                             :auth-provider (getf options :auth-provider)
                             :headers (getf options :headers))
                           (list :request-fields request-fields)))))
      (remember-client client)
      (make-agent :client client
                  :workspace (or (getf options :workspace) (uiop:getcwd))))))

(defun run-once (prompt agent)
  "--print: run PROMPT, then save the chat. Returns the exit status."
  (let ((warning (compact-model-warning (agent-client agent))))
    (when warning (format *error-output* "~&~A~%" (styled :dim warning))))
  (let* ((*ui-context-wrapper* (and *conversation* (conversation-ui-wrapper *conversation*)))
         (status (handler-case (progn (deliver-conversation-notes *conversation*)
                                            (run prompt :agent agent) 0)
                  (error (condition)
                    (report-error "maxwell: ~A" condition)
                    1))))
    (if (save-session :quiet t) status 1)))

(defun main (&optional (arguments (uiop:command-line-arguments)))
  "Run Maxwell with command-line ARGUMENTS. Returns the exit status. This
thread is the UI thread: it owns the terminal, reads keys, and draws; turns and
compaction run on workers of their own."
  (claim-ui-thread)
  ;; Colour and the transcript belong to this terminal session, never to a
  ;; saved one.
  (reset-transcript)
  (configure-color :auto)
  (handler-case
      (let ((options (parse-options arguments)))
        (when (getf options :color) (configure-color (getf options :color)))
        (cond
          ((getf options :help) (usage) 0)
          ((getf options :verify-resume)
           (read-resume-state (getf options :verify-resume))
           (format t "Verified ~D conversations.~%" (length *conversations*)) 0)
          ((getf options :export-chat) (export-chat) 0)
          ((getf options :logout-provider)
           (logout (getf options :logout-provider))
           0)
          ((getf options :login-provider)
           (let ((client (login-and-adopt (getf options :login-provider)
                                          (make-client :model ""))))
             (format t "Maxwell will use ~A with ~A; run maxwell to start.~%"
                     (client-base-url client) (client-model client)))
           0)
          (t
           (open-global-chat options arguments)
           (when (getf options :resume-state)
             (read-resume-state (getf options :resume-state)))
           (if (getf options :adopt-chat)
               (adopt-global-chat)
               (let ((agent (make-session-agent options)))
                 (when *conversations*
                   (let ((selected (or *selected-conversation* (first *conversations*))))
                     (when (or (getf options :model) (getf options :auth-provider)
                               (getf options :base-url) (getf options :workspace))
                       (setf (conversation-agent selected) agent))
                     (setf agent (conversation-agent selected))))
                 (setf *agent* agent)
                 (ensure-conversations agent)
                 (if (getf options :prompt)
                     (run-once (getf options :prompt) agent)
                     (interactive-loop agent)))))))
    (error (condition)
      (report-error "maxwell: ~A" condition)
      1)))

(defun image-main ()
  "The entry point of the built executable and of every saved image: run
MAIN on the command line and exit with its status."
  ;; An image is saved with no threads running, so nothing of a previous
  ;; process survives into the new one: the UI claim is taken again by MAIN,
  ;; and the queues start empty. A saved chat keeps its own compaction, so
  ;; the new session installs its own tick in START-BACKGROUND-WORK.
  (release-ui-thread)
  (reset-conversation-runtime)
  (setf *workers* '()
        *compactor-worker* nil
        *compactor-tick* nil
        *compactor-run* nil
        *ui-queue* nil
        ;; A signal handler does not survive being saved: install it again.
        *interrupt-handler-installed* nil
        *interrupt-requested* nil
        *current-turn* nil)
  (sb-ext:exit :code (main) :abort nil))






