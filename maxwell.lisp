#!/usr/bin/env -S sbcl --script

(require "asdf")
(let* ((root (uiop:pathname-directory-pathname *load-truename*))
       (cache (uiop:ensure-directory-pathname
                (merge-pathnames ".cache/common-lisp/" root))))
  (ensure-directories-exist cache)
  (asdf:initialize-output-translations
    `(:output-translations (t ,cache) :ignore-inherited-configuration)))
(asdf:load-asd (merge-pathnames "maxwell.asd"
                                (uiop:pathname-directory-pathname *load-truename*)))
(asdf:load-system "maxwell")
(uiop:quit (maxwell:main))
