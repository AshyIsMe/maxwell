(defpackage #:maxwell/json
  (:use #:cl)
  (:export #:decode
           #:encode
           #:get-key
           #:json-object
           #:json-array
           #:+null+))

(defpackage #:maxwell
  (:use #:cl)
  (:import-from #:maxwell/json
                #:decode
                #:encode
                #:get-key
                #:json-object
                #:json-array
                #:+null+)
  (:export #:main
           #:run
           #:make-agent
           #:make-client
           #:eval-lisp
           #:request-image
           #:save-image
           #:workspace-path
           #:*agent*
           #:agent-history
           #:agent-workspace
           #:client-model
           #:client-base-url
           #:client-endpoint
           #:client-wire-api
           #:client-extra-headers))

(defpackage #:maxwell/workspace
  (:use #:cl)
  (:documentation "The package in which Maxwell evaluates agent-authored Lisp."))
