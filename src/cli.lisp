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
    (:prompt "-p" "--print") (:color "--color"))
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
              :extra-headers (or headers (getf remembered :extra-headers)))))))

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
  (setf (client-base-url client) (trim-slashes (safeai-base-url))
        (client-endpoint client) nil
        (client-wire-api client) :chat-completions
        (client-auth-provider client) :safeai
        (client-api-key-environment client) +safeai-api-key-environment+
        ;; Headers and request fields belonged to the previous provider; the
        ;; ChatGPT plan's stream:true in particular would break Chat Completions.
        (client-extra-headers client) '()
        (client-request-fields client) nil
        (client-model client) model)
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

(defun print-model-catalog (agent)
  (let* ((client (agent-client agent))
         (models (list-provider-models client)))
    (format t "~&Current model: ~A~%~A~%"
            (client-model client)
            (styled :dim (format nil "Available from ~A:" (client-base-url client))))
    (dolist (model models)
      (format t "  ~A~@[ ~A~]~%" (car model)
              (unless (string= (car model) (cdr model))
                (styled :dim (format nil "— ~A" (cdr model))))))
    models))

(defun select-model (agent model-id)
  (when (zerop (length model-id))
    (error "Usage: /model MODEL"))
  (let ((models (list-provider-models (agent-client agent))))
    (unless (find model-id models :key #'car :test #'string=)
      (error "Model ~S is not available from the current provider." model-id))
    (setf (client-model (agent-client agent)) model-id)
    (remember-client (agent-client agent))
    (format t "~A~%" (styled :dim (format nil "Selected model: ~A" model-id)))))

(defun run-interactive-turn (line agent)
  "Run LINE, keeping the prompt live while the model works so that the user can
browse the transcript and queue steering prompts. The compactor keeps working
meanwhile."
  (let ((state (interactive-terminal-state)))
    (if (null state)
        (run line :agent agent)
        (unwind-protect
             (progn
               (terminal-command "-icanon" "-echo" "min" "1" "time" "0")
               (let* ((session (make-steering-session agent *standard-input*
                                                      *standard-output*))
                      (*input-poll* (lambda ()
                                      (service-steering-input session)
                                      (tick-compactor agent)))
                      (*input-hide* (lambda () (steering-hide session))))
                 (unwind-protect (run line :agent agent)
                   (steering-hide session)
                   ;; An unfinished steering line becomes the next prompt.
                   (setf *prompt-draft* (steering-draft session)))))
          (terminal-command state)))))

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
  "Print an error message on *ERROR-OUTPUT* in the :error face."
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
/model         list current provider's available models
/model MODEL   select an available model
/memory        show the chat's size, summaries, and compactor jobs
/import PATH   read a file into memory as a note
/checkpoint    save the chat to the global image and keep going
/reload        load Maxwell's source into this session, then save
/save PATH     save a separate resumable image and exit
/quit          save the chat and exit (Ctrl-D too)
Editing: Up/Down history; Ctrl-A/E line ends; Ctrl-U/W/K kill; Ctrl-Y yank.
Ctrl-T expands or collapses tool calls; PgUp/PgDn browse the transcript.
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

(defun interactive-loop (&optional (agent *agent*))
  (setf *agent* (or agent (make-agent)))
  (unless *chat* (setf *chat* (make-chat)))
  (let ((start (chat-message-count *chat*)))
    (setf *older-entries-function* (older-log-entries *chat* start))
    ;; A resumed chat shows where it left off.
    (when (plusp start)
      (show-recent-exchanges (recent-log-entries *chat*) :count 2)))
  (record-entry :status
                (format nil "Maxwell ~A — ~A~%Type /help for commands; PgUp/PgDn browse the transcript, Ctrl-T expands tool calls, Up/Down recall previous prompts."
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
         (handler-case (print-model-catalog *agent*)
           (error (condition)
             (report-error "Could not list models: ~A" condition))))
        ((command-argument line "/model ")
         (handler-case
             (select-model *agent* (command-argument line "/model "))
           (error (condition)
             (report-error "Could not select model: ~A" condition))))
        ((command-p line "/memory")
         (status "~A~%image: ~A" (memory-report *chat*)
                 (if *checkpoint-path* (native *checkpoint-path*) "none (ephemeral)")))
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
                 (save-image (namestring (workspace-path path)))))))
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
       (setf *chat* (make-chat)))
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
  (let ((status (handler-case (progn (run prompt :agent agent) 0)
                  (error (condition)
                    (report-error "maxwell: ~A" condition)
                    1))))
    (if (save-session :quiet t) status 1)))

(defun main (&optional (arguments (uiop:command-line-arguments)))
  "Run Maxwell with command-line ARGUMENTS. Returns the exit status."
  ;; Colour and the transcript belong to this terminal session, never to a
  ;; saved one.
  (reset-transcript)
  (configure-color :auto)
  (handler-case
      (let ((options (parse-options arguments)))
        (when (getf options :color) (configure-color (getf options :color)))
        (cond
          ((getf options :help) (usage) 0)
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
           (if (getf options :adopt-chat)
               (adopt-global-chat)
               (let ((agent (make-session-agent options)))
                 (setf *agent* agent)
                 (if (getf options :prompt)
                     (run-once (getf options :prompt) agent)
                     (interactive-loop agent)))))))
    (error (condition)
      (report-error "maxwell: ~A" condition)
      1)))

(defun image-main ()
  "The entry point of the built executable and of every saved image: run
MAIN on the command line and exit with its status."
  (sb-ext:exit :code (main) :abort nil))
