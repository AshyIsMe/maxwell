(asdf:defsystem "maxwell"
  :description "A small, image-persistent Common Lisp coding agent harness."
  :author "Maxwell contributors"
  :license "MIT"
  :version "0.1.0"
  :depends-on ("uiop")
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
               (:file "src/session")
               (:file "src/memory")
               (:file "src/compactor")
               (:file "src/render")
               (:file "src/agent")
               (:file "src/terminal")
               (:file "src/cli"))
  :build-operation "program-op"
  :build-pathname "maxwell"
  :entry-point "maxwell:main"
  :in-order-to ((test-op (test-op "maxwell/tests"))))

(asdf:defsystem "maxwell/tests"
  :depends-on ("maxwell")
  :serial t
  :components ((:file "tests/package")
               (:file "tests/support")
               (:file "tests/ui-tests")
               (:file "tests/memory-tests")
               (:file "tests/tests"))
  :perform (test-op (operation component)
             (declare (ignore operation component))
             (unless (uiop:symbol-call :maxwell/tests :run-tests)
               (error "Maxwell tests failed."))))
