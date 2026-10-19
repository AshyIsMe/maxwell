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
  ;; FSet shadows many CL names (FIND, REMOVE, UNION, ...), so it is reached
  ;; through a nickname rather than :USEd, like Alexandria.
  (:local-nicknames (#:a #:alexandria) (#:f #:fset))
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
           #:zoom
           #:date
           #:*chat*
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
  (:local-nicknames (#:a #:alexandria) (#:f #:fset))
  (:documentation "The package in which Maxwell evaluates agent-authored Lisp."))
