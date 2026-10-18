(in-package #:maxwell)

(defun usage (&optional (stream *standard-output*))
  (write-string "Maxwell — a small Common Lisp coding agent

Usage: maxwell [options] [prompt ...]

  -m, --model MODEL       model name (or MAXWELL_MODEL)
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
  -h, --help              show this help

Environment: OPENAI_API_KEY, MAXWELL_MODEL, MAXWELL_BASE_URL,
             MAXWELL_ENDPOINT, MAXWELL_WIRE_API, MAXWELL_API_KEY_ENV,
             MAXWELL_SESSION_FILE, MAXWELL_OAUTH_MODE (auto, manual, loopback).
  openai:    MAXWELL_AUTH_FILE, MAXWELL_CODEX_MODEL (default gpt-5.6-sol).
  safeai:    SAFEAILABS_API_KEY (overrides the saved key),
             MAXWELL_SAFEAI_AUTH_FILE, MAXWELL_SAFEAI_BASE_URL,
             MAXWELL_SAFEAI_MODEL.

Without a provider option, Maxwell reuses the provider and model of the
previous session, remembered in ~/.config/maxwell/session.json.
" stream))

(defun take-option (arguments option)
  (unless arguments (error "~A needs a value." option))
  (values (first arguments) (rest arguments)))

(defun parse-options (arguments)
  (let ((model nil) (base-url nil) (endpoint nil) (wire-api nil) (api-key-env nil)
        (auth-provider nil) (login-provider nil) (logout-provider nil)
        (headers '()) (request-json nil) (workspace nil) (prompt nil) (help nil)
        (color nil))
    (loop while arguments
          for argument = (pop arguments)
          do (cond
               ((member argument '("-h" "--help") :test #'string=)
                (setf help t))
               ((member argument '("-m" "--model") :test #'string=)
                (multiple-value-setq (model arguments) (take-option arguments argument)))
               ((member argument '("-b" "--base-url") :test #'string=)
                (multiple-value-setq (base-url arguments) (take-option arguments argument)))
               ((member argument '("-e" "--endpoint") :test #'string=)
                (multiple-value-setq (endpoint arguments) (take-option arguments argument)))
               ((member argument '("-w" "--wire-api") :test #'string=)
                (multiple-value-setq (wire-api arguments) (take-option arguments argument)))
               ((member argument '("-k" "--api-key-env") :test #'string=)
                (multiple-value-setq (api-key-env arguments) (take-option arguments argument)))
               ((member argument '("-a" "--auth") :test #'string=)
                (multiple-value-setq (auth-provider arguments) (take-option arguments argument)))
               ((string= argument "--login")
                (multiple-value-setq (login-provider arguments) (take-option arguments argument)))
               ((string= argument "--logout")
                (multiple-value-setq (logout-provider arguments) (take-option arguments argument)))
               ((member argument '("-H" "--header") :test #'string=)
                (multiple-value-bind (header rest) (take-option arguments argument)
                  (push header headers)
                  (setf arguments rest)))
               ((string= argument "--request-json")
                (multiple-value-setq (request-json arguments) (take-option arguments argument)))
               ((member argument '("-C" "--workspace") :test #'string=)
                (multiple-value-setq (workspace arguments) (take-option arguments argument)))
               ((member argument '("-p" "--print") :test #'string=)
                (multiple-value-setq (prompt arguments) (take-option arguments argument)))
               ((string= argument "--color")
                (multiple-value-setq (color arguments) (take-option arguments argument)))
               ((and (plusp (length argument)) (char= (char argument 0) #\-))
                (error "Unknown option ~A." argument))
               (t
                (setf prompt (format nil "~{~A~^ ~}" (cons argument arguments))
                      arguments nil))))
    (values model base-url endpoint wire-api api-key-env auth-provider login-provider
            (nreverse headers) request-json workspace prompt help logout-provider color)))

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

(defun resolve-client-options (&key model base-url endpoint wire-api api-key-env
                                    auth-provider headers
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
browse the transcript and queue steering prompts."
  (let ((state (interactive-terminal-state)))
    (if (null state)
        (run line :agent agent)
        (unwind-protect
             (progn
               (terminal-command "-icanon" "-echo" "min" "1" "time" "0")
               (let* ((session (make-steering-session agent *standard-input*
                                                      *standard-output*))
                      (*input-poll* (lambda () (service-steering-input session)))
                      (*input-hide* (lambda () (steering-hide session))))
                 (unwind-protect (run line :agent agent)
                   (steering-hide session)
                   ;; An unfinished steering line becomes the next prompt.
                   (setf *prompt-draft* (steering-draft session)))))
          (terminal-command state)))))

(defun interactive-loop (&optional (agent *agent*))
  (setf *agent* (or agent (make-agent)))
  (record-entry :status
                (format nil "Maxwell ~A — ~A~%Type /help for commands; PgUp/PgDn browse the transcript, Ctrl-T expands tool calls, Up/Down recall previous prompts."
                        (client-model (agent-client *agent*))
                        (namestring (agent-workspace *agent*)))
                :show t)
  (loop
    (let ((line (read-interactive-prompt *agent*)))
      (unless line (return 0))
      (cond
        ((or (command-p line "/quit") (command-p line "/exit")) (return 0))
        ((command-p line "/help")
         (write-line (style-lines :dim (format nil "/login openai  sign in with ChatGPT OAuth~%/login safeai  save a Safe AI Labs API key~%/logout NAME   forget a saved login~%/model         list current provider's available models~%/model MODEL   select an available model~%/save PATH      save a resumable executable image and exit~%/history        show message count~%/quit           exit~%Editing: Up/Down history; Ctrl-A/E line ends; Ctrl-U/W/K kill; Ctrl-Y yank.~%Ctrl-T expands or collapses tool calls; PgUp/PgDn browse the transcript.~%While the model works: type a line and press Enter to steer the model~%before its next request."))))
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
        ((command-p line "/history")
         (format t "~D messages~%" (length (agent-history *agent*))))
        ((command-argument line "/save ")
         (let ((path (command-argument line "/save ")))
           (if (zerop (length path))
               (format t "Usage: /save PATH~%")
               (save-image (namestring (workspace-path path))))))
        ((zerop (length (string-trim '(#\Space #\Tab) line))))
        (t
         (handler-case (run-interactive-turn line *agent*)
           (error (condition)
             (record-entry :status (format nil "Error: ~A" condition) :ok-p nil :show t))))))))

(defun report-error (control &rest arguments)
  "Print an error message on *ERROR-OUTPUT* in the :error face."
  (format *error-output* "~&~A~%" (styled :error (apply #'format nil control arguments))))

(defun color-option (arguments)
  "The value of --color in ARGUMENTS, or :auto."
  (or (second (member "--color" arguments :test #'string=)) :auto))

(defun image-main ()
  ;; Colour and the transcript belong to this terminal session, not the saved one.
  (reset-transcript)
  (handler-case (configure-color (color-option (uiop:command-line-arguments)))
    (error () (configure-color :auto)))
  (handler-case (interactive-loop *agent*)
    (error (condition)
      (report-error "Fatal error: ~A" condition)
      1)))

(defun main (&optional (arguments (uiop:command-line-arguments)))
  (reset-transcript)
  (configure-color :auto)
  (handler-case
      (multiple-value-bind (model base-url endpoint wire-api api-key-env auth-provider
                            login-provider headers request-json workspace prompt help
                            logout-provider color)
          (parse-options arguments)
        (when color (configure-color color))
        (when help (usage) (return-from main 0))
        (when logout-provider
          (logout logout-provider)
          (return-from main 0))
        (when login-provider
          (let ((client (login-and-adopt login-provider (make-client :model ""))))
            (format t "Maxwell will use ~A with ~A; run maxwell to start.~%"
                    (client-base-url client) (client-model client)))
          (return-from main 0))
        (let ((request-fields
                (and request-json
                     (handler-case (decode request-json)
                       (error (condition)
                         (error "Invalid --request-json: ~A" condition))))))
          (when (and request-fields (not (hash-table-p request-fields)))
            (error "--request-json must be a JSON object."))
        (let* ((client (apply #'make-client
                              (append
                                (resolve-client-options
                                  :model model
                                  :base-url base-url
                                  :endpoint endpoint
                                  :wire-api wire-api
                                  :api-key-env api-key-env
                                  :auth-provider auth-provider
                                  :headers headers)
                                (list :request-fields request-fields))))
               (agent (make-agent :client client
                                  :workspace (or workspace (uiop:getcwd)))))
          (remember-client client)
          (setf *agent* agent)
          (if prompt
              (progn (run prompt :agent agent) 0)
              (interactive-loop agent)))))
    (error (condition)
      (report-error "maxwell: ~A" condition)
      1)))
