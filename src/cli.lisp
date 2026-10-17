(in-package #:maxwell)

(defun usage (&optional (stream *standard-output*))
  (write-string "Maxwell — a small Common Lisp coding agent

Usage: maxwell [options] [prompt ...]

  -m, --model MODEL       model name (or MAXWELL_MODEL)
  -b, --base-url URL      OpenAI-compatible /v1 URL
  -e, --endpoint URL      exact API endpoint URL
  -w, --wire-api API      chat (default) or responses
  -k, --api-key-env NAME  environment variable holding the API key
  -a, --auth PROVIDER     saved OAuth provider (currently openai)
      --login PROVIDER    sign in and save OAuth credentials, then exit
  -H, --header HEADER     add a provider HTTP header (repeatable)
      --request-json JSON add provider-specific request fields
  -C, --workspace DIR     project directory
  -p, --print PROMPT      run once and exit
  -h, --help              show this help

Environment: OPENAI_API_KEY, MAXWELL_MODEL, MAXWELL_BASE_URL,
             MAXWELL_ENDPOINT, MAXWELL_WIRE_API, MAXWELL_API_KEY_ENV,
             MAXWELL_AUTH_FILE, MAXWELL_CODEX_MODEL.
             MAXWELL_OAUTH_MODE (auto, manual, or loopback).
" stream))

(defun take-option (arguments option)
  (unless arguments (error "~A needs a value." option))
  (values (first arguments) (rest arguments)))

(defun parse-options (arguments)
  (let ((model nil) (base-url nil) (endpoint nil) (wire-api nil) (api-key-env nil)
        (auth-provider nil) (login-provider nil)
        (headers '()) (request-json nil) (workspace nil) (prompt nil) (help nil))
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
               ((and (plusp (length argument)) (char= (char argument 0) #\-))
                (error "Unknown option ~A." argument))
               (t
                (setf prompt (format nil "~{~A~^ ~}" (cons argument arguments))
                      arguments nil))))
    (values model base-url endpoint wire-api api-key-env auth-provider login-provider
            (nreverse headers) request-json workspace prompt help)))

(defun command-p (line name)
  (string= (string-trim '(#\Space #\Tab) line) name))

(defun command-argument (line prefix)
  (when (and (>= (length line) (length prefix))
             (string= line prefix :end1 (length prefix) :end2 (length prefix)))
    (string-trim '(#\Space #\Tab) (subseq line (length prefix)))))

(defun print-model-catalog (agent)
  (let* ((client (agent-client agent))
         (models (list-provider-models client)))
    (format t "~&Current model: ~A~%Available from ~A:~%"
            (client-model client) (client-base-url client))
    (dolist (model models)
      (format t "  ~A~@[ — ~A~]~%" (car model)
              (unless (string= (car model) (cdr model)) (cdr model))))
    models))

(defun select-model (agent model-id)
  (when (zerop (length model-id))
    (error "Usage: /model MODEL"))
  (let ((models (list-provider-models (agent-client agent))))
    (unless (find model-id models :key #'car :test #'string=)
      (error "Model ~S is not available from the current provider." model-id))
    (setf (client-model (agent-client agent)) model-id)
    (format t "Selected model: ~A~%" model-id)))

(defun interactive-loop (&optional (agent *agent*))
  (setf *agent* (or agent (make-agent)))
  (format t "~&Maxwell ~A — ~A~%Type /help for commands.~%"
          (client-model (agent-client *agent*))
          (namestring (agent-workspace *agent*)))
  (loop
    (format t "~&maxwell> ")
    (finish-output)
    (let ((line (read-line *standard-input* nil nil)))
      (unless line (return 0))
      (cond
        ((or (command-p line "/quit") (command-p line "/exit")) (return 0))
        ((command-p line "/help")
         (format t "/login openai  sign in with ChatGPT OAuth~%/model         list current provider's available models~%/model MODEL   select an available model~%/save PATH      save a resumable executable image and exit~%/history        show message count~%/quit           exit~%"))
        ((command-p line "/login openai")
         (handler-case
             (progn
               (openai-login)
               (let ((client (agent-client *agent*)))
                 (configure-client-for-openai-oauth client)
                 (unless (and (string= (client-model client)
                                       (or (uiop:getenv "MAXWELL_CODEX_MODEL") ""))
                              (plusp (length (client-model client))))
                   (setf (client-model client)
                         (or (uiop:getenv "MAXWELL_CODEX_MODEL")
                             "gpt-5.6-sol"))))
               (format t "The current Maxwell session now uses OpenAI OAuth.~%"))
           (error (condition)
             (format *error-output* "Login failed: ~A~%" condition))))
        ((command-p line "/model")
         (handler-case (print-model-catalog *agent*)
           (error (condition)
             (format *error-output* "Could not list models: ~A~%" condition))))
        ((command-argument line "/model ")
         (handler-case
             (select-model *agent* (command-argument line "/model "))
           (error (condition)
             (format *error-output* "Could not select model: ~A~%" condition))))
        ((command-p line "/history")
         (format t "~D messages~%" (length (agent-history *agent*))))
        ((command-argument line "/save ")
         (let ((path (command-argument line "/save ")))
           (if (zerop (length path))
               (format t "Usage: /save PATH~%")
               (save-image (namestring (workspace-path path))))))
        ((zerop (length (string-trim '(#\Space #\Tab) line))))
        (t
         (handler-case (run line :agent *agent*)
           (error (condition) (format *error-output* "Error: ~A~%" condition))))))))

(defun image-main ()
  (handler-case (interactive-loop *agent*)
    (error (condition)
      (format *error-output* "Fatal error: ~A~%" condition)
      1)))

(defun main (&optional (arguments (uiop:command-line-arguments)))
  (handler-case
      (multiple-value-bind (model base-url endpoint wire-api api-key-env auth-provider
                            login-provider headers request-json workspace prompt help)
          (parse-options arguments)
        (when help (usage) (return-from main 0))
        (when login-provider
          (unless (string-equal login-provider "openai")
            (error "Unknown login provider ~S; supported provider: openai."
                   login-provider))
          (openai-login)
          (return-from main 0))
        (let ((request-fields
                (and request-json
                     (handler-case (decode request-json)
                       (error (condition)
                         (error "Invalid --request-json: ~A" condition))))))
          (when (and request-fields (not (hash-table-p request-fields)))
            (error "--request-json must be a JSON object."))
        (let* ((client (make-client
                         :model (or model (uiop:getenv "MAXWELL_MODEL") "")
                         :base-url (or base-url (uiop:getenv "MAXWELL_BASE_URL")
                                       "https://api.openai.com/v1")
                         :endpoint (or endpoint (uiop:getenv "MAXWELL_ENDPOINT"))
                         :wire-api (or wire-api (uiop:getenv "MAXWELL_WIRE_API")
                                       :chat-completions)
                         :api-key-environment
                         (or api-key-env (uiop:getenv "MAXWELL_API_KEY_ENV")
                             "OPENAI_API_KEY")
                         :auth-provider
                         (cond
                           ((null auth-provider) nil)
                           ((string-equal auth-provider "openai") :openai)
                           (t (error "Unknown auth provider ~S; supported provider: openai."
                                     auth-provider)))
                         :extra-headers headers
                         :request-fields request-fields))
               (agent (make-agent :client client
                                  :workspace (or workspace (uiop:getcwd)))))
          (setf *agent* agent)
          (if prompt
              (progn (run prompt :agent agent) 0)
              (interactive-loop agent)))))
    (error (condition)
      (format *error-output* "maxwell: ~A~%" condition)
      1)))
