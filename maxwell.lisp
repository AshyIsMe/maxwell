#!/usr/bin/env -S sbcl --script

(require "asdf")
(let* ((root (uiop:pathname-directory-pathname *load-truename*))
       (cache (uiop:ensure-directory-pathname
                (merge-pathnames ".cache/common-lisp/" root))))
  (ensure-directories-exist cache)
  ;; Mirror the project tree inside the cache. A flat cache would make
  ;; src/package.lisp and tests/package.lisp share one fasl, so compiling the
  ;; tests would break the next startup.
  (asdf:initialize-output-translations
    `(:output-translations
       (,(merge-pathnames "**/*.*" root) ,(merge-pathnames "**/*.*" cache))
       (t ,cache)
       :ignore-inherited-configuration)))
(asdf:load-asd (merge-pathnames "maxwell.asd"
                                (uiop:pathname-directory-pathname *load-truename*)))
(asdf:load-system "maxwell")
(uiop:quit (maxwell:main))
