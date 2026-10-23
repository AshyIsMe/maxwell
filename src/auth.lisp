(in-package #:maxwell)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (require :sb-bsd-sockets))

(defparameter +openai-authorize-url+
  "https://auth.openai.com/api/accounts/authorize")
(defparameter +openai-token-url+
  "https://auth.openai.com/api/accounts/oauth/token")
(defparameter +openai-jwks-url+
  "https://auth.openai.com/.well-known/jwks.json")
(defparameter +openai-resource+ "https://api.openai.com/v1")
(defparameter +openai-issuer+ "https://auth.openai.com")
(defparameter +openai-scope+
  "openid profile email offline_access resource.invoke chatgpt.tokens.use.direct")

(defun environment-value (name)
  "The value of environment variable NAME, or NIL when it is unset or empty."
  (let ((value (uiop:getenv name)))
    (and value (plusp (length value)) value)))

(defun environment-pathname (name)
  "The pathname held by environment variable NAME, or NIL."
  (let ((value (environment-value name)))
    (and value (pathname value))))

(defun config-directory ()
  "Maxwell's per-user configuration directory."
  (uiop:ensure-directory-pathname
    (or (environment-pathname "XDG_CONFIG_HOME")
        (merge-pathnames ".config/" (user-homedir-pathname)))))

(defvar *auth-file* nil
  "When non-NIL, the file holding the OpenAI OAuth credential.")

(defun auth-file-path ()
  (or *auth-file*
      (environment-pathname "MAXWELL_AUTH_FILE")
      (merge-pathnames "maxwell/openai-auth.json" (config-directory))))

(defun random-octets (count)
  (let ((bytes (make-array count :element-type '(unsigned-byte 8))))
    (with-open-file (stream #P"/dev/urandom" :direction :input
                                             :element-type '(unsigned-byte 8))
      (unless (= count (read-sequence bytes stream))
        (error "Could not read secure random bytes.")))
    bytes))

(defparameter +base64-alphabet+
  "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/")

(defun base64-encode (bytes &key url (padding (not url)))
  (let ((text
          (with-output-to-string (stream)
            (loop for offset from 0 below (length bytes) by 3
                  for remaining = (- (length bytes) offset)
                  for a = (aref bytes offset)
                  for b = (if (> remaining 1) (aref bytes (1+ offset)) 0)
                  for c = (if (> remaining 2) (aref bytes (+ offset 2)) 0)
                  for bits = (logior (ash a 16) (ash b 8) c)
                  do (write-char (char +base64-alphabet+ (ldb (byte 6 18) bits)) stream)
                     (write-char (char +base64-alphabet+ (ldb (byte 6 12) bits)) stream)
                     (when (> remaining 1)
                       (write-char (char +base64-alphabet+ (ldb (byte 6 6) bits)) stream))
                     (when (> remaining 2)
                       (write-char (char +base64-alphabet+ (ldb (byte 6 0) bits)) stream))))))
    (when padding
      (setf text (concatenate 'string text
                              (make-string (mod (- 4 (mod (length text) 4)) 4)
                                           :initial-element #\=))))
    (if url
        (substitute #\_ #\/ (substitute #\- #\+ text))
        text)))

(defun base64-value (character)
  (position character +base64-alphabet+))

(defun base64url-decode (text)
  (let* ((standard (substitute #\/ #\_ (substitute #\+ #\- text)))
         (unpadded (string-right-trim "=" standard))
         (result (make-array (floor (* (length unpadded) 6) 8)
                             :element-type '(unsigned-byte 8)))
         (bits 0)
         (bit-count 0)
         (position 0))
    (loop for character across unpadded
          for value = (base64-value character)
          do (unless value (error "Invalid base64url data."))
             (setf bits (logior (ash bits 6) value)
                   bit-count (+ bit-count 6))
             (when (>= bit-count 8)
               (decf bit-count 8)
               (setf (aref result position) (ldb (byte 8 bit-count) bits))
               (incf position)
               (setf bits (ldb (byte bit-count 0) bits))))
    result))

(defun random-value (&optional (bytes 32))
  (base64-encode (random-octets bytes) :url t))

(defun uuid-v4 ()
  (let ((bytes (random-octets 16)))
    (setf (aref bytes 6) (logior #x40 (logand #x0f (aref bytes 6)))
          (aref bytes 8) (logior #x80 (logand #x3f (aref bytes 8))))
    (let ((hex (with-output-to-string (stream)
                 (loop for byte across bytes do (format stream "~2,'0x" byte)))))
      (format nil "~A-~A-~A-~A-~A"
              (subseq hex 0 8) (subseq hex 8 12) (subseq hex 12 16)
              (subseq hex 16 20) (subseq hex 20 32)))))

(defun hex-octets (text)
  (let ((bytes (make-array (/ (length text) 2)
                           :element-type '(unsigned-byte 8))))
    (loop for index below (length bytes)
          for offset = (* index 2)
          do (setf (aref bytes index)
                   (parse-integer text :start offset :end (+ offset 2)
                                        :radix 16)))
    bytes))

(defun pkce-challenge (verifier)
  (multiple-value-bind (output stderr status)
      (uiop:run-program '("openssl" "dgst" "-sha256" "-r")
                        :input (make-string-input-stream verifier)
                        :output :string :error-output :string
                        :ignore-error-status t)
    (unless (zerop status)
      (error "Could not generate PKCE challenge: ~A" stderr))
    (let ((hex (subseq output 0 (min 64 (length output)))))
      (unless (and (= (length hex) 64)
                   (every (lambda (character) (digit-char-p character 16)) hex))
        (error "OpenSSL returned an invalid SHA-256 digest."))
      (base64-encode (hex-octets hex) :url t))))

(defun url-encode (value)
  (with-output-to-string (stream)
    (loop for character across value
          for code = (char-code character)
          do (if (or (alphanumericp character) (find character "-._~"))
                 (write-char character stream)
                 (format stream "%~2,'0X" code)))))

(defun form-encode (pairs)
  (format nil "~{~A~^&~}"
          (loop for (name . value) in pairs
                collect (format nil "~A=~A" (url-encode name)
                                             (url-encode value)))))

(defun url-decode (text)
  (with-output-to-string (stream)
    (loop with position = 0
          while (< position (length text))
          for character = (char text position)
          do (cond
               ((char= character #\%)
                (when (> (+ position 3) (length text))
                  (error "Truncated URL escape."))
                (write-char (code-char (parse-integer text :start (1+ position)
                                                           :end (+ position 3)
                                                           :radix 16))
                            stream)
                (incf position 3))
               ((char= character #\+)
                (write-char #\Space stream)
                (incf position))
               (t (write-char character stream) (incf position))))))

(defun query-parameters (target)
  (let ((question (position #\? target)))
    (values (if question (subseq target 0 question) target)
            (when question
              (loop for part in (uiop:split-string (subseq target (1+ question))
                                                   :separator '(#\&))
                    for equals = (position #\= part)
                    collect (cons (url-decode (if equals (subseq part 0 equals) part))
                                  (url-decode (if equals (subseq part (1+ equals)) ""))))))))

(defun unix-time ()
  (- (get-universal-time) 2208988800))

(defun load-auth-record (&optional (path (auth-file-path)))
  (when (probe-file path)
    (handler-case
        (let ((record (decode (uiop:read-file-string path))))
          (unless (hash-table-p record)
            (error "credential file is not a JSON object"))
          record)
      (error (condition)
        (error "Could not read Maxwell credentials at ~A: ~A" path condition)))))

(defun protect-file (path)
  (uiop:run-program (list "chmod" "600" (namestring path))
                    :output nil :error-output nil))

(defun write-private-json-file (record path)
  "Atomically write RECORD to PATH as JSON readable only by its owner."
  (ensure-directories-exist path)
  (let ((temporary
          (make-pathname :name (format nil ".~A.tmp-~A"
                                      (or (pathname-name path) "maxwell")
                                      (random-value 8))
                         :type (pathname-type path)
                         :defaults path)))
    (unwind-protect
         (progn
           (with-open-file (stream temporary :direction :output
                                             :if-exists :error
                                             :if-does-not-exist :create)
             (write-string (encode record) stream)
             (terpri stream))
           (protect-file temporary)
           (uiop:rename-file-overwriting-target temporary path)
           (protect-file path))
      (when (probe-file temporary) (delete-file temporary))))
  record)

(defun save-auth-record (record &optional (path (auth-file-path)))
  (write-private-json-file record path))

(defun ensure-host-record ()
  (let ((record (or (load-auth-record) (json-object))))
    (unless (stringp (get-key record "ext_agent_host_id"))
      (setf (gethash "ext_agent_host_id" record)
            (format nil "urn:uuid:~A" (uuid-v4)))
      (save-auth-record record))
    record))

(defun oauth-post-form (url pairs)
  (let ((body (form-encode pairs)))
    (with-input-from-string (input body)
      (multiple-value-bind (output stderr status)
          (uiop:run-program
            (list "curl" "--silent" "--show-error" "--location"
                  "--max-time" "60"
                  "--header" "Accept: application/json"
                  "--header" "Content-Type: application/x-www-form-urlencoded"
                  "--data-binary" "@-" "--write-out" (format nil "~%%{http_code}")
                  url)
            :input input :output :string :error-output :string
            :ignore-error-status t)
        (let* ((newline (position #\Newline output :from-end t))
               (response-body (if newline (subseq output 0 newline) output))
               (http-status (if newline (subseq output (1+ newline)) "000")))
          (unless (and (zerop status) (= (length http-status) 3)
                       (char= (char http-status 0) #\2))
            (error "OpenAI OAuth request failed (HTTP/process status ~A): ~A~@[~%~A~]"
                   (if (zerop status) http-status status) response-body
                   (and (plusp (length stderr)) stderr)))
          (handler-case (decode response-body)
            (error (condition)
              (error "OpenAI OAuth endpoint returned invalid JSON: ~A" condition))))))))

(defun der-length (length)
  (if (< length 128)
      (vector length)
      (let ((bytes '()))
        (loop while (plusp length)
              do (push (logand length #xff) bytes)
                 (setf length (ash length -8)))
        (coerce (cons (logior #x80 (length bytes)) bytes)
                '(vector (unsigned-byte 8))))))

(defun octet-concatenate (&rest vectors)
  (let* ((length (reduce #'+ vectors :key #'length))
         (result (make-array length :element-type '(unsigned-byte 8)))
         (position 0))
    (dolist (vector vectors result)
      (replace result vector :start1 position)
      (incf position (length vector)))))

(defun der-object (tag content)
  (octet-concatenate (vector tag) (der-length (length content)) content))

(defun der-integer (bytes)
  (let* ((first-nonzero (or (position-if-not #'zerop bytes) (1- (length bytes))))
         (value (subseq bytes first-nonzero))
         (positive (if (logbitp 7 (aref value 0))
                       (octet-concatenate #(0) value)
                       value)))
    (der-object #x02 positive)))

(defun rsa-public-key-pem (modulus exponent)
  (let* ((rsa-key (der-object #x30
                              (octet-concatenate (der-integer modulus)
                                                 (der-integer exponent))))
         (algorithm #(#x30 #x0d #x06 #x09 #x2a #x86 #x48 #x86 #xf7
                      #x0d #x01 #x01 #x01 #x05 #x00))
         (bit-string (der-object #x03 (octet-concatenate #(0) rsa-key)))
         (spki (der-object #x30 (octet-concatenate algorithm bit-string)))
         (encoded (base64-encode spki)))
    (with-output-to-string (stream)
      (write-line "-----BEGIN PUBLIC KEY-----" stream)
      (loop for start from 0 below (length encoded) by 64
            do (write-line (subseq encoded start (min (+ start 64) (length encoded)))
                           stream))
      (write-line "-----END PUBLIC KEY-----" stream))))

(defun jwt-parts (token)
  (let ((parts (uiop:split-string token :separator '(#\.))))
    (unless (= (length parts) 3) (error "ID token is not a JWT."))
    parts))

(defun decode-jwt-part (part)
  (decode (sb-ext:octets-to-string (base64url-decode part)
                                    :external-format :utf-8)))

(defun write-octets-file (path bytes)
  (with-open-file (stream path :direction :output :if-exists :error
                                :if-does-not-exist :create
                                :element-type '(unsigned-byte 8))
    (write-sequence bytes stream))
  (protect-file path))

(defun verify-jwt-signature (token header)
  (unless (string= (get-key header "alg" "") "RS256")
    (error "Unsupported OpenAI ID-token algorithm ~S (expected RS256)."
           (get-key header "alg")))
  (let* ((kid (get-key header "kid"))
         (jwks-output
           (uiop:run-program (list "curl" "--silent" "--show-error" "--fail"
                                   "--max-time" "30" +openai-jwks-url+)
                             :output :string :error-output :string))
         (keys (get-key (decode jwks-output) "keys"))
         (jwk (and (vectorp keys)
                   (find kid keys :test #'string=
                                  :key (lambda (key) (get-key key "kid" ""))))))
    (unless (and jwk (string= (get-key jwk "kty" "") "RSA"))
      (error "OpenAI JWKS has no RSA key matching ID-token kid ~S." kid))
    (destructuring-bind (encoded-header encoded-payload encoded-signature)
        (jwt-parts token)
      (let* ((stem (format nil "/tmp/maxwell-jwt-~A" (random-value 10)))
             (public-path (pathname (concatenate 'string stem ".pem")))
             (signature-path (pathname (concatenate 'string stem ".sig")))
             (data-path (pathname (concatenate 'string stem ".data"))))
        (unwind-protect
             (progn
               (with-open-file (stream public-path :direction :output
                                                   :if-exists :error
                                                   :if-does-not-exist :create)
                 (write-string
                   (rsa-public-key-pem (base64url-decode (get-key jwk "n"))
                                       (base64url-decode (get-key jwk "e")))
                   stream))
               (protect-file public-path)
               (write-octets-file signature-path (base64url-decode encoded-signature))
               (with-open-file (stream data-path :direction :output
                                                 :if-exists :error
                                                 :if-does-not-exist :create)
                 (format stream "~A.~A" encoded-header encoded-payload))
               (protect-file data-path)
               (multiple-value-bind (output stderr status)
                   (uiop:run-program
                     (list "openssl" "dgst" "-sha256" "-verify"
                           (namestring public-path) "-signature"
                           (namestring signature-path) (namestring data-path))
                     :output :string :error-output :string :ignore-error-status t)
                 (declare (ignore output))
                 (unless (zerop status)
                   (error "OpenAI ID-token signature verification failed: ~A" stderr))))
          (dolist (path (list public-path signature-path data-path))
            (when (probe-file path) (delete-file path))))))))

(defun audience-matches-p (audience client-id)
  (or (and (stringp audience) (string= audience client-id))
      (and (vectorp audience) (find client-id audience :test #'string=))))

(defun validate-id-token (token client-id nonce)
  (destructuring-bind (header-part payload-part signature-part) (jwt-parts token)
    (declare (ignore signature-part))
    (let ((header (decode-jwt-part header-part))
          (claims (decode-jwt-part payload-part)))
      (verify-jwt-signature token header)
      (unless (string= (get-key claims "iss" "") +openai-issuer+)
        (error "OpenAI ID-token issuer did not match."))
      (unless (audience-matches-p (get-key claims "aud") client-id)
        (error "OpenAI ID-token audience did not match the issued client ID."))
      (unless (and (numberp (get-key claims "exp"))
                   (> (get-key claims "exp") (- (unix-time) 5)))
        (error "OpenAI ID token is expired."))
      (unless (and (numberp (get-key claims "iat"))
                   (<= (get-key claims "iat") (+ (unix-time) 5)))
        (error "OpenAI ID token has an invalid issued-at time."))
      (unless (string= (get-key claims "nonce" "") nonce)
        (error "OpenAI ID-token nonce did not match."))
      (unless (and (stringp (get-key claims "sub"))
                   (plusp (length (get-key claims "sub"))))
        (error "OpenAI ID token has no subject."))
      claims)))

(defun start-oauth-listener ()
  (require :sb-bsd-sockets)
  (let ((socket (make-instance 'sb-bsd-sockets:inet-socket
                               :type :stream :protocol :tcp)))
    (handler-case
        (progn
          (sb-bsd-sockets:socket-bind socket #(127 0 0 1) 0)
          (sb-bsd-sockets:socket-listen socket 1)
          (multiple-value-bind (address port) (sb-bsd-sockets:socket-name socket)
            (declare (ignore address))
            (values socket port)))
      (error (condition)
        (sb-bsd-sockets:socket-close socket)
        (error condition)))))

(defun send-callback-page (stream status heading message)
  (let ((body (format nil "<!doctype html><title>~A</title><h1>~A</h1><p>~A</p>"
                      heading heading message)))
    (format stream "HTTP/1.1 ~D ~A~C~CContent-Type: text/html; charset=utf-8~C~CCache-Control: no-store~C~CContent-Length: ~D~C~CConnection: close~C~C~C~C~A"
            status (if (= status 200) "OK" "Bad Request") #\Return #\Newline
            #\Return #\Newline #\Return #\Newline (length body)
            #\Return #\Newline #\Return #\Newline #\Return #\Newline body)
    (finish-output stream)))

(defun oauth-result-from-parameters (parameters expected-state expected-client-id)
  (let ((state (cdr (assoc "state" parameters :test #'string=)))
        (oauth-error (cdr (assoc "error" parameters :test #'string=)))
        (code (cdr (assoc "code" parameters :test #'string=)))
        (returned-client-id
          (cdr (assoc "client_id" parameters :test #'string=))))
    (unless (and state (string= state expected-state))
      (error "OAuth state did not match."))
    (when oauth-error
      (error "OpenAI authorization failed: ~A" oauth-error))
    (unless code (error "OAuth callback contained no code."))
    (let ((client-id (or returned-client-id expected-client-id)))
      (unless client-id
        (error "OAuth callback contained no issued client ID."))
      (when (and expected-client-id returned-client-id
                 (not (string= expected-client-id returned-client-id)))
        (error "OAuth callback returned a different client ID."))
      (values code client-id))))

(defun oauth-result-from-pasted-url (input redirect-uri expected-state
                                     expected-client-id)
  (let ((url (string-trim '(#\Space #\Tab #\Return #\Newline) input)))
    (multiple-value-bind (callback-uri parameters) (query-parameters url)
      (unless (string= callback-uri redirect-uri)
        (error "Paste the complete callback URL beginning with ~A" redirect-uri))
      (oauth-result-from-parameters parameters expected-state expected-client-id))))

(defun await-oauth-callback (listener expected-state expected-client-id)
  (multiple-value-bind (client address port)
      (sb-bsd-sockets:socket-accept listener)
    (declare (ignore address port))
    (unwind-protect
         (let ((stream (sb-bsd-sockets:socket-make-stream
                         client :input t :output t :buffering :none
                                :element-type 'character :external-format :utf-8)))
           (unwind-protect
                (handler-case
                    (let* ((request-line (read-line stream nil ""))
                           (first-space (position #\Space request-line))
                           (second-space (and first-space
                                              (position #\Space request-line
                                                        :start (1+ first-space))))
                           (target (and second-space
                                        (subseq request-line (1+ first-space)
                                                             second-space))))
                      (loop for line = (read-line stream nil "")
                            until (zerop (length (string-right-trim '(#\Return) line))))
                      (unless target (error "Malformed OAuth callback request."))
                      (multiple-value-bind (path parameters) (query-parameters target)
                        (unless (string= path "/auth/callback")
                          (error "Unexpected OAuth callback path."))
                        (multiple-value-prog1
                            (oauth-result-from-parameters parameters expected-state
                                                          expected-client-id)
                          (send-callback-page stream 200 "Authentication successful"
                                              "Maxwell is connected. You can close this window."))))
                  (error (condition)
                    (ignore-errors
                      (send-callback-page stream 400 "Authentication failed"
                                          "Return to Maxwell for details."))
                    (error condition)))
             (close stream)))
      (ignore-errors (sb-bsd-sockets:socket-close client)))))

(defun open-browser (url)
  (or (ignore-errors
        (uiop:launch-program (list "xdg-open" url) :output nil :error-output nil)
        t)
      (progn (format t "Open this URL in your browser:~%~A~%" url) nil)))

(defun manual-oauth-p ()
  (let ((mode (string-downcase (or (uiop:getenv "MAXWELL_OAUTH_MODE") "auto"))))
    (cond
      ((string= mode "manual") t)
      ((string= mode "loopback") nil)
      ((string= mode "auto")
       (or (uiop:getenv "SSH_CONNECTION") (uiop:getenv "SSH_TTY")))
      (t (error "MAXWELL_OAUTH_MODE must be auto, manual, or loopback.")))))

(defun receive-oauth-result (listener redirect-uri authorization-url expected-state
                             expected-client-id manual)
  (if manual
      (progn
        (format t "~&Remote login mode: open this URL on your local computer:~%~A~%~%After approval, the browser may show that 127.0.0.1 refused the connection.~%Copy the browser's entire final URL and paste it here:~%> "
                authorization-url)
        (finish-output)
        (let ((callback-url (read-line *standard-input* nil nil)))
          (unless callback-url (error "No callback URL was provided."))
          (oauth-result-from-pasted-url callback-url redirect-uri expected-state
                                        expected-client-id)))
      (progn
        (format t "~&Continue with ChatGPT in your browser.~%If it does not open, use:~%~A~%~%Waiting for the callback on ~A ...~%"
                authorization-url redirect-uri)
        (finish-output)
        (open-browser authorization-url)
        (await-oauth-callback listener expected-state expected-client-id))))

(defun random-oauth-port ()
  (let ((bytes (random-octets 2)))
    (+ 49152 (mod (+ (ash (aref bytes 0) 8) (aref bytes 1)) 16384))))

(defun require-token-field (tokens field)
  (let ((value (get-key tokens field)))
    (unless (and (stringp value) (plusp (length value)))
      (error "OpenAI token response has no valid ~A." field))
    value))

(defun scope-present-p (scope required)
  (find required (uiop:split-string scope :separator '(#\Space #\Tab #\Newline))
        :test #'string=))

(defun openai-login ()
  "Complete Sign in with ChatGPT and save an owner-only OAuth credential file."
  (let* ((record (ensure-host-record))
         (existing-client-id (get-key record "client_id"))
         (verifier (random-value))
         (challenge (pkce-challenge verifier))
         (state (random-value))
         (nonce (random-value))
         (manual (manual-oauth-p)))
    (multiple-value-bind (listener port)
        (if manual (values nil (random-oauth-port)) (start-oauth-listener))
      (unwind-protect
           (let* ((redirect-uri (format nil "http://127.0.0.1:~D/auth/callback" port))
                  (parameters
                    (append
                      (list (cons "client_id" (or existing-client-id
                                                  "dynamic_agent_client")))
                      (unless existing-client-id
                        (list (cons "agent_name_hint" "Maxwell")))
                      (list (cons "ext_agent_host_id"
                                  (get-key record "ext_agent_host_id"))
                            (cons "response_type" "code")
                            (cons "redirect_uri" redirect-uri)
                            (cons "resource" +openai-resource+)
                            (cons "scope" +openai-scope+)
                            (cons "state" state)
                            (cons "code_challenge" challenge)
                            (cons "code_challenge_method" "S256")
                            (cons "nonce" nonce))))
                  (url (format nil "~A?~A" +openai-authorize-url+
                               (form-encode parameters))))
             (multiple-value-bind (code client-id)
                 (receive-oauth-result listener redirect-uri url state existing-client-id
                                       manual)
               (let* ((tokens
                        (oauth-post-form
                          +openai-token-url+
                          (list (cons "grant_type" "authorization_code")
                                (cons "client_id" client-id)
                                (cons "code" code)
                                (cons "code_verifier" verifier)
                                (cons "redirect_uri" redirect-uri)
                                (cons "resource" +openai-resource+))))
                      (access-token (require-token-field tokens "access_token"))
                      (refresh-token (require-token-field tokens "refresh_token"))
                      (id-token (require-token-field tokens "id_token"))
                      (scope (require-token-field tokens "scope"))
                      (expires-in (get-key tokens "expires_in"))
                      (claims (validate-id-token id-token client-id nonce)))
                 (unless (scope-present-p scope "chatgpt.tokens.use.direct")
                   (error "ChatGPT plan usage was not granted."))
                 (unless (and (numberp expires-in) (plusp expires-in))
                   (error "OpenAI token response has invalid expires_in."))
                 (setf (gethash "client_id" record) client-id
                       (gethash "subject" record) (get-key claims "sub")
                       (gethash "issuer" record) (get-key claims "iss")
                       (gethash "email" record) (or (get-key claims "email") "")
                       (gethash "id_token" record) id-token
                       (gethash "access_token" record) access-token
                       (gethash "refresh_token" record) refresh-token
                       (gethash "scope" record) scope
                       (gethash "expires_at" record) (+ (unix-time) expires-in))
                 (save-auth-record record)
                 (format t "~&Signed in with ChatGPT. Credentials saved to ~A.~%"
                         (auth-file-path))
                 access-token)))
        (when listener (sb-bsd-sockets:socket-close listener))))))

(defun refresh-openai-token (record)
  (let* ((client-id (get-key record "client_id"))
         (refresh-token (get-key record "refresh_token")))
    (unless (and (stringp client-id) (stringp refresh-token))
      (error "Maxwell has no refreshable OpenAI login. Run /login openai."))
    (let* ((tokens
             (oauth-post-form +openai-token-url+
                              (list (cons "grant_type" "refresh_token")
                                    (cons "client_id" client-id)
                                    (cons "refresh_token" refresh-token)
                                    (cons "resource" +openai-resource+))))
           (access-token (require-token-field tokens "access_token"))
           (new-refresh-token (or (get-key tokens "refresh_token") refresh-token))
           (scope (or (get-key tokens "scope") (get-key record "scope")))
           (expires-in (get-key tokens "expires_in")))
      (unless (and (stringp scope)
                   (scope-present-p scope "chatgpt.tokens.use.direct"))
        (error "Refreshed OpenAI credential lacks ChatGPT plan permission."))
      (unless (and (numberp expires-in) (plusp expires-in))
        (error "OpenAI refresh response has invalid expires_in."))
      (setf (gethash "access_token" record) access-token
            (gethash "refresh_token" record) new-refresh-token
            (gethash "scope" record) scope
            (gethash "expires_at" record) (+ (unix-time) expires-in))
      (save-auth-record record)
      access-token)))

(defun openai-access-token ()
  (let ((record (load-auth-record)))
    (unless record
      (error "Maxwell is not signed in to OpenAI. Run /login openai or --login openai."))
    (let ((token (get-key record "access_token"))
          (expires-at (get-key record "expires_at" 0)))
      (if (and (stringp token) (numberp expires-at)
               (> expires-at (+ (unix-time) 300)))
          token
          (refresh-openai-token record)))))

(defun forget-openai-login (&optional (path (auth-file-path)))
  "Remove the saved ChatGPT tokens. The host ID and issued client ID stay, so a
later login registers as the same Maxwell installation."
  (let ((record (load-auth-record path)))
    (when record
      (dolist (field '("id_token" "access_token" "refresh_token" "scope"
                       "expires_at" "subject" "issuer" "email"))
        (remhash field record))
      (save-auth-record record path)
      t)))

;;; Safe AI Labs offers no OAuth for third-party clients; its own web sign-in
;;; issues keys that work only from its Sail CLI. Maxwell therefore uses a
;;; browser-assisted API-key login: it opens the key page, the user creates a
;;; key there and pastes it, and Maxwell verifies and saves it owner-only.

(defparameter +safeai-keys-url+ "https://safeailabs.ai/keys")
(defparameter +safeai-base-url+ "https://us-region.api.safeailabs.ai/v1")
(defparameter +safeai-api-key-environment+ "SAFEAILABS_API_KEY")

(defvar *safeai-auth-file* nil
  "When non-NIL, the file holding the saved Safe AI Labs API key.")

(defun safeai-auth-file-path (&optional (environment #'environment-value))
  "The owner-only file holding the Safe AI Labs API key saved by --login safeai."
  (or *safeai-auth-file*
      (let ((value (funcall environment "MAXWELL_SAFEAI_AUTH_FILE")))
        (and value (pathname value)))
      (merge-pathnames "maxwell/safeai-auth.json" (config-directory))))

(defun load-safeai-auth-record (&optional (path (safeai-auth-file-path)))
  (load-auth-record path))

(defun safeai-api-key (&key (environment #'environment-value)
                            (path (safeai-auth-file-path)))
  "The Safe AI Labs API key: SAFEAILABS_API_KEY when set, else the saved login."
  (or (funcall environment +safeai-api-key-environment+)
      (let ((key (get-key (or (load-safeai-auth-record path) (json-object))
                          "api_key")))
        (and (stringp key) (plusp (length key)) key))
      (error "Maxwell has no Safe AI Labs API key. Run /login safeai or maxwell --login safeai.")))

(defun safeai-key-problem (key)
  "A reason KEY cannot be an API key, or NIL. Keys travel in an HTTP header, so
anything but visible ASCII is refused outright."
  (cond
    ((zerop (length key)) "No API key was entered.")
    ((notevery (lambda (character) (char< #\Space character (code-char 127))) key)
     "The API key contains spaces or characters that cannot appear in a key.")))

(defun safeai-response-code (body)
  "The error code in a Safe AI Labs error BODY, or NIL."
  (let* ((json (ignore-errors (decode body)))
         (error-object (and (hash-table-p json) (get-key json "error")))
         (code (cond ((hash-table-p error-object) (get-key error-object "code"))
                     ((hash-table-p json) (get-key json "code")))))
    (and (stringp code) code)))

(defun safeai-key-check-message (http-status body &optional (curl-status 0))
  "Explain why verifying a Safe AI Labs key failed, or return NIL when the
HTTP-STATUS of GET /models shows the key works."
  (let ((code (or (safeai-response-code body)
                  (and (search "sail_key_requires_sail" body)
                       "sail_key_requires_sail"))))
    (cond
      ((not (zerop curl-status))
       (format nil "Could not reach Safe AI Labs (curl exit status ~A)." curl-status))
      ((and (= (length http-status) 3) (char= (char http-status 0) #\2)) nil)
      ((equal code "sail_key_requires_sail")
       (format nil "That key belongs to the Sail CLI and works only from Sail. Create an API key at ~A and paste that instead."
               +safeai-keys-url+))
      ((string= http-status "401")
       (format nil "Safe AI Labs rejected the key~@[ (~A)~]. Copy the complete key from ~A and try again."
               code +safeai-keys-url+))
      ((string= http-status "403")
       (format nil "Safe AI Labs refused the key~@[ (~A)~]. Check its permissions at ~A."
               code +safeai-keys-url+))
      ((string= http-status "429")
       (format nil "Safe AI Labs is rate limiting this key~@[ (~A)~]. Its quota may be used up; check it at ~A, or wait and try again."
               code +safeai-keys-url+))
      ((string= http-status "000")
       "Could not reach Safe AI Labs.")
      (t
       (format nil "Safe AI Labs returned HTTP ~A~@[ (~A)~] while checking the key."
               http-status code)))))

(defun read-secret-line (prompt)
  "Read a line after PROMPT without echoing it when standard input is a terminal."
  (format t "~A" prompt)
  (finish-output)
  (let ((state (interactive-terminal-state)))
    (prog1 (if state
               (unwind-protect
                    (progn
                      (terminal-command "-echo")
                      (read-line *standard-input* nil nil))
                 (terminal-command state))
               (read-line *standard-input* nil nil))
      ;; Neither the line nor its Enter was echoed.
      (terpri)
      (finish-output))))

(defun safeai-base-url (&optional (environment #'environment-value))
  (or (funcall environment "MAXWELL_SAFEAI_BASE_URL") +safeai-base-url+))

(defun safeai-login ()
  "Open the Safe AI Labs key page, read a pasted API key, verify it, and save it
owner-only. Returns the model to use: MAXWELL_SAFEAI_MODEL or the first listed."
  (if (manual-oauth-p)
      (format t "~&Open this page on your local computer and sign in:~%~A~%"
              +safeai-keys-url+)
      (progn
        (format t "~&Opening Safe AI Labs in your browser. If it does not open, visit:~%~A~%"
                +safeai-keys-url+)
        (finish-output)
        (open-browser +safeai-keys-url+)))
  (format t "~%Create an API key there (it starts with sail_), copy it, and paste it below.~%")
  (let* ((input (or (read-secret-line "API key (hidden): ")
                    (error "No API key was entered.")))
         (key (string-trim '(#\Space #\Tab #\Return #\Newline) input))
         (base-url (safeai-base-url)))
    (let ((problem (safeai-key-problem key)))
      (when problem (error problem)))
    (unless (uiop:string-prefix-p "sail_" key)
      (format t "Warning: Safe AI Labs API keys normally start with sail_.~%"))
    (format t "Checking the key with ~A ...~%" base-url)
    (finish-output)
    (multiple-value-bind (body http-status curl-status)
        (fetch-models-response base-url key)
      (let ((problem (safeai-key-check-message http-status body curl-status)))
        (when problem (error problem)))
      (let* ((catalog (model-catalog-from-body body))
             (model (or (environment-value "MAXWELL_SAFEAI_MODEL")
                        (car (first catalog))
                        (error "Safe AI Labs listed no models. Set MAXWELL_SAFEAI_MODEL.")))
             (path (safeai-auth-file-path)))
        (when (and catalog (not (find model catalog :key #'car :test #'string=)))
          (format t "Warning: MAXWELL_SAFEAI_MODEL is ~A, which Safe AI Labs does not list. Available: ~{~A~^, ~}.~%"
                  model (mapcar #'car catalog)))
        (write-private-json-file
          (json-object "api_key" key
                       "base_url" base-url
                       "model" model
                       "saved_at" (unix-time))
          path)
        (format t "Safe AI Labs key verified. Saved to ~A.~%" path)
        (let ((environment-key (environment-value +safeai-api-key-environment+)))
          (when (and environment-key (string/= environment-key key))
            (format t "Note: ~A is set and takes precedence over the saved key.~%"
                    +safeai-api-key-environment+)))
        model))))

(defun forget-safeai-login (&optional (path (safeai-auth-file-path)))
  "Delete the saved Safe AI Labs key. True when there was one."
  (when (probe-file path)
    (delete-file path)
    t))

;;; Which providers this installation can use: /model lists these and can
;;; switch between them.

(defun openai-login-saved-p (&optional (path (auth-file-path)))
  "True when a saved ChatGPT login can still supply an access token."
  (let ((record (handler-case (load-auth-record path) (error () nil))))
    (and (hash-table-p record)
         (let ((token (or (get-key record "access_token")
                          (get-key record "refresh_token"))))
           (and (stringp token) (plusp (length token)))))))

(defun safeai-login-saved-p (&optional (environment #'environment-value))
  "True when a Safe AI Labs key is saved or set in the environment."
  (handler-case
      (let ((key (safeai-api-key :environment environment)))
        (and (stringp key) (plusp (length key)) t))
    (error () nil)))
