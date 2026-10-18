(in-package #:maxwell)

;;; Maxwell remembers the provider configuration and model of the previous
;;; session, so that a bare invocation continues with the same service and
;;; model as the last one. The record is a default, never an override:
;;; command-line options and MAXWELL_* environment variables always win, and a
;;; missing or unreadable file simply means "nothing remembered".
;;;
;;; Only the provider's identity is stored. Credentials keep coming from the
;;; environment variable named by the provider, or from the saved OAuth login.

(defparameter +session-version+ 1)

(defvar *session-file* nil
  "When non-NIL, the file Maxwell uses to remember the previous session.")

(defun session-file-path ()
  "The file holding the provider and model of the previous session."
  (or *session-file*
      (environment-pathname "MAXWELL_SESSION_FILE")
      (merge-pathnames "maxwell/session.json" (config-directory))))

(defun load-session-record (&optional (path (session-file-path)))
  "Return the remembered session object, or NIL when there is none to trust."
  (when (probe-file path)
    (handler-case
        (let ((record (decode (uiop:read-file-string path))))
          (and (hash-table-p record) record))
      (error () nil))))

(defun session-string (object key)
  (let ((value (and object (get-key object key))))
    (and (stringp value) (plusp (length value)) value)))

(defun session-keyword (object key)
  (let ((value (session-string object key)))
    (and value (intern (string-upcase value) :keyword))))

(defun session-provider (record)
  (let ((provider (and record (get-key record "provider"))))
    (and (hash-table-p provider) provider)))

(defun session-headers (provider)
  (let ((headers (and provider (get-key provider "headers"))))
    (when (vectorp headers)
      (loop for header across headers
            when (stringp header) collect header))))

(defun remembered-client-options (record)
  "The MAKE-CLIENT arguments remembered in RECORD, or NIL when it names no provider."
  (let ((provider (session-provider record)))
    (when provider
      (list :base-url (session-string provider "base_url")
            :endpoint (session-string provider "endpoint")
            :wire-api (session-keyword provider "wire_api")
            :api-key-environment (session-string provider "api_key_env")
            :auth-provider (session-keyword provider "auth_provider")
            :extra-headers (session-headers provider)
            :model (session-string record "model")))))

(defun session-record-for-client (client)
  "Describe CLIENT's provider and model as a JSON object."
  (json-object
    "version" +session-version+
    "model" (client-model client)
    "provider"
    (json-object
      "base_url" (client-base-url client)
      "endpoint" (or (client-endpoint client) +null+)
      "wire_api" (string-downcase (symbol-name (client-wire-api client)))
      "api_key_env" (client-api-key-environment client)
      "auth_provider" (let ((provider (client-auth-provider client)))
                         (if provider
                             (string-downcase (symbol-name provider))
                             +null+))
      "headers" (coerce (client-extra-headers client) 'vector))))

(defun remember-client (client &optional (path (session-file-path)))
  "Remember CLIENT's provider and model as the default for the next session.
Remembering is a convenience, so an unwritable session file is not an error."
  (handler-case
      (let ((record (session-record-for-client client)))
        (unless (equalp record (load-session-record path))
          (write-private-json-file record path)))
    (error () nil))
  client)
