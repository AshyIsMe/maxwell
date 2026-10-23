;;; Run from the project root:
;;; sbcl --script tests/image-save.lisp /tmp/maxwell-save-test.image [request]
;;; Then run the resulting image with --help to check that it resumes.
(require :asdf)
(asdf:load-asd (truename "maxwell.asd"))
(asdf:load-system :maxwell)
(maxwell::claim-ui-thread)
(let* ((args (uiop:command-line-arguments))
       (path (or (first args) (error "Supply a temporary output image path.")))
       (maxwell::*session-entries* (maxwell::make-growable-vector)))
  (maxwell::record-entry :status "must survive a failed save" :show nil)
  (maxwell::start-compactor-worker (lambda () nil))
  ;; Force failure before the snapshot can be written. A failed save must
  ;; leave the transcript and the compactor usable.
  (assert (handler-case (progn (maxwell::save-image "/dev/null/impossible") nil)
            (error () t)))
  (assert (= 1 (length maxwell::*session-entries*)))
  (assert (maxwell::compactor-worker-live-p))
  ;; Also exercise a failure after the transcript has been cleared for saving.
  (let ((directory (concatenate 'string path ".failure/")))
    (ensure-directories-exist directory)
    (unwind-protect
         (assert (handler-case (progn (maxwell::save-image directory) nil)
                   (error () t)))
      (sb-posix:rmdir directory)))
  (assert (= 1 (length maxwell::*session-entries*)))
  (assert (maxwell::compactor-worker-live-p))
  ;; Both public save entry points must stop the live compactor before saving.
  (if (equal (second args) "request")
      (progn (maxwell:request-image path) (maxwell::finish-pending-image))
      (maxwell::save-image path)))
