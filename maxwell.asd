;;; Maxwell's Lisp libraries are vendored under vendor/ at pinned versions
;;; (see vendor/fetch.sh), so the build, `sbcl --script maxwell.lisp`, and
;;; /reload never depend on what else is installed. The central registry is
;;; searched before ASDF's source registry, so these copies win over any
;;; other installed version, and the user's own registry is left alone.
(dolist (directory (directory
                     (merge-pathnames "vendor/*/"
                                      (make-pathname :name nil :type nil
                                                     :defaults *load-truename*))))
  (pushnew directory asdf:*central-registry* :test #'equal))

(asdf:defsystem "maxwell"
  :description "A small, image-persistent Common Lisp coding agent harness."
  :author "Maxwell contributors"
  :license "MIT"
  :version "0.1.0"
  :depends-on ("uiop" "alexandria" "fset")
  :serial t
  :components ((:file "src/package")
               (:file "src/json")
               (:file "src/runtime")
               (:file "src/style")
               (:file "src/highlight")
               (:file "src/markdown")
               (:file "src/auth")
               (:file "src/process")
               (:file "src/provider")
               (:file "src/status")
               (:file "src/session")
               (:file "src/memory")
               (:file "src/compactor")
               (:file "src/render")
               (:file "src/agent")
               (:file "src/terminal")
               (:file "src/image")
               (:file "src/cli"))
  :build-operation "program-op"
  :build-pathname "maxwell"
  :entry-point "maxwell::image-main"
  :in-order-to ((test-op (test-op "maxwell/tests"))))

(asdf:defsystem "maxwell/tests"
  :depends-on ("maxwell")
  :serial t
  :components ((:file "tests/package")
               (:file "tests/support")
               (:file "tests/ui-tests")
               (:file "tests/memory-tests")
               (:file "tests/session-tests")
               (:file "tests/tests"))
  :perform (test-op (operation component)
             (declare (ignore operation component))
             (unless (uiop:symbol-call :maxwell/tests :run-tests)
               (error "Maxwell tests failed."))))
