(in-package #:maxwell/tests)

;;; Threads. The UI thread owns the terminal, the transcript, and the chat; work
;;; runs on workers. These tests pin the handoff rules down without a terminal.

(defun test-ui-queue ()
  ;; With no UI thread claimed, everything runs where it is called, as in a
  ;; one-shot run or in any other test.
  (check (maxwell::ui-thread-p))
  (let ((ran nil))
    (check (maxwell::ui-post (lambda () (setf ran :posted))))
    (check (eq :posted ran))
    (check (eql 42 (maxwell::ui-call (lambda () 42)))))
  ;; With a worker calling, UI work runs on the UI thread and its values come
  ;; back to the worker.
  (let* ((maxwell::*ui-thread* (bt:current-thread))
         (seen (list nil)))
    (let ((worker (maxwell::start-worker
                    "queue test"
                    (lambda ()
                      (push (maxwell::ui-call
                              (lambda () (list :in-ui (maxwell::ui-thread-p))))
                            (first seen))
                      (maxwell::ui-post (lambda () (push :posted (first seen))))))))
      ;; This thread is the UI thread: it answers the worker's call.
      (loop until (first seen) do (maxwell::drain-ui-queue) (sleep 0.01))
      (maxwell::worker-join worker)
      (maxwell::drain-ui-queue)
      (check (member '(:in-ui t) (first seen) :test #'equal))
      (check (member :posted (first seen)))
      (check (null (maxwell::live-workers))))
    ;; An error signalled in UI work reaches the worker that asked for it.
    (let ((outcome (list nil)))
      (let ((worker (maxwell::start-worker
                      "error test"
                      (lambda ()
                        (setf (first outcome)
                              (handler-case
                                  (maxwell::ui-call (lambda () (error "boom")))
                                (error (condition) (princ-to-string condition))))))))
        (loop until (first outcome) do (maxwell::drain-ui-queue) (sleep 0.01))
        (maxwell::worker-join worker)
        (check (search "boom" (first outcome)))))))

(defun test-workers ()
  ;; Workers are listed while alive and gone afterwards.
  (let* ((release (bt:make-semaphore :count 0))
         (worker (maxwell::start-worker
                   "listed" (lambda () (bt:wait-on-semaphore release)))))
    (check (member worker (maxwell::live-workers)))
    (check (maxwell::worker-alive-p worker))
    (bt:signal-semaphore release)
    (maxwell::worker-join worker)
    (check (null (maxwell::live-workers))))
  ;; QUIESCE-WORKERS waits for workers that end in time, and stops the rest.
  (let ((worker (maxwell::start-worker "waits" (lambda () (sleep 0.3)))))
    (check (maxwell::quiesce-workers :timeout 3))
    (check (not (maxwell::worker-alive-p worker)))))

(defun test-interrupt-flag ()
  ;; Ctrl-C only raises a flag; whoever owns the moment decides what it means.
  (let ((maxwell::*interrupt-requested* nil))
    (check (null (maxwell::take-interrupt)))
    (maxwell::request-interrupt)
    (check (maxwell::take-interrupt))
    (check (null (maxwell::take-interrupt)))))

(defun test-threaded-turn ()
  ;; A turn on a worker leaves the calling thread free -- it runs queued UI
  ;; work while the model thinks -- and its reply still comes back.
  (let* ((maxwell:*chat* (maxwell::make-chat))
         (maxwell::*session-entries* (maxwell::make-entry-vector))
         (maxwell::*ui-thread* (bt:current-thread))
         (maxwell::*show-entries* nil)
         (maxwell::*in-turn* nil)
         (calls 0)
         (pumped 0)
         (agent (maxwell:make-agent
                  :model-function (lambda (messages tools)
                                    (declare (ignore messages tools))
                                    (incf calls)
                                    (sleep 0.1)
                                    (fake-response :content "threaded reply")))))
    (let ((turn (maxwell::start-turn "hello" :agent agent :print-response nil)))
      ;; While the worker sleeps in the model, this thread can do UI work.
      (maxwell::ui-post (lambda () (incf pumped)))
      (check (string= "threaded reply" (maxwell::await-turn turn))))
    (check (= 1 calls))
    (check (= 1 pumped))
    ;; The worker's entries reached this thread's transcript.
    (check (equal '(:user :assistant)
                  (map 'list #'maxwell::entry-kind maxwell::*session-entries*)))
    (check (equal '(:user :maxwell)
                  (map 'list #'maxwell::message-kind
                         (maxwell::chat-messages maxwell:*chat*)))))
  ;; A cancelled turn is abandoned rather than waited for.
  (let* ((maxwell:*chat* (maxwell::make-chat))
         (maxwell::*session-entries* (maxwell::make-entry-vector))
         (maxwell::*ui-thread* (bt:current-thread))
         (maxwell::*in-turn* nil)
         (slow (maxwell:make-agent
                 :model-function (lambda (messages tools)
                                   (declare (ignore messages tools))
                                   (sleep 30)
                                   (fake-response :content "never"))))
         (turn (maxwell::start-turn "slow" :agent slow :print-response nil)))
    (sleep 0.05)
    (check (maxwell::cancel-turn turn))
    (sleep 0.05)
    (check (not (maxwell::worker-alive-p (maxwell::turn-worker turn))))))

(defun test-steering-shared-queue ()
  ;; A prompt queued on the UI thread reaches a turn running on a worker: the
  ;; worker must use the same steering queue, not a binding of its own.
  (let* ((maxwell:*chat* (maxwell::make-chat))
         (maxwell::*session-entries* (maxwell::make-entry-vector))
         (maxwell::*ui-thread* (bt:current-thread))
         (maxwell::*show-entries* nil)
         (maxwell::*in-turn* nil)
         (first-reply (list nil))
         (agent (maxwell:make-agent
                  :model-function (lambda (messages tools)
                                    (declare (ignore messages tools))
                                    (if (first first-reply)
                                        (fake-response :content "steered")
                                        (progn (setf (first first-reply) t)
                                               ;; Answer once the steering
                                               ;; prompt is queued, so it
                                               ;; arrives mid-turn.
                                               (loop until (maxwell::steering-pending-p)
                                                     do (sleep 0.005))
                                               (fake-response :content "first")))))))
    (let ((turn (maxwell::start-turn "go" :agent agent :print-response nil)))
      ;; This thread is the UI thread: it answers the worker until the model
      ;; has been asked, then queues a prompt as the steering line would.
      (loop until (first first-reply) do (maxwell::drain-ui-queue) (sleep 0.005))
      (maxwell::queue-steering "steer")
      (check (string= "steered" (maxwell::await-turn turn))))
    (check (equal '(:user :maxwell :user :maxwell)
                  (map 'list #'maxwell::message-kind
                         (maxwell::chat-messages maxwell:*chat*))))))
(defun test-compactor-worker ()
  ;; The compactor's worker stops when asked, and starts again after a
  ;; checkpoint has stopped it -- both by itself, so a quiesce-before-fork
  ;; does not have to destroy it in the middle of a tick.
  (let* ((ticks 0)
         (lock (bt:make-lock "ticks"))
         (tick (lambda () (bt:with-lock-held (lock) (incf ticks))))
         (maxwell::*compactor-worker* nil)
         (maxwell::*compactor-tick* nil))
    (check (null (maxwell::compactor-worker-live-p)))
    ;; Restarting with no function to run does nothing, as in a one-shot run.
    (check (null (maxwell::restart-compactor-worker)))
    (maxwell::start-compactor-worker tick)
    (check (maxwell::compactor-worker-live-p))
    (sleep 0.4)
    (check (plusp ticks))
    (maxwell::stop-compactor-worker)
    (check (null (maxwell::compactor-worker-live-p)))
    (check (null (maxwell::live-workers)))
    (let ((before ticks))
      (maxwell::restart-compactor-worker)
      (check (maxwell::compactor-worker-live-p))
      (sleep 0.4)
      (check (> ticks before))
      (maxwell::stop-compactor-worker))
    (setf maxwell::*compactor-tick* nil)))

(defun test-checkpoint-needs-ui-thread ()
  ;; The save forks, and cannot beside a worker: a worker that asked for one
  ;; gets an error that says to request an image instead.
  (let* ((maxwell::*checkpoint-path* #p"/nonexistent/maxwell.image")
         (maxwell::*ui-thread* (bt:current-thread))
         (outcome (list nil)))
    (let ((worker (maxwell::start-worker
                    "checkpoint"
                    (lambda ()
                      (setf (first outcome)
                            (handler-case (maxwell::checkpoint)
                              (error (condition) (princ-to-string condition)))))
                    ;; A worker inherits no dynamic bindings: it is told which
                    ;; image path the session has.
                    :bindings (list (cons 'maxwell::*checkpoint-path*
                                          maxwell::*checkpoint-path*)))))
      (loop until (first outcome) do (maxwell::drain-ui-queue) (sleep 0.01))
      (maxwell::worker-join worker)
      (check (search "request-image" (first outcome))))))


