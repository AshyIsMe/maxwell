(in-package #:maxwell)

;;; Threads, through Bordeaux Threads. Maxwell used to do everything on one
;;; thread: provider requests ran as polled child processes so that the prompt
;;; could keep reading keys between polls. Now the main thread -- the UI thread
;;; -- does nothing but read keys and draw, and every long piece of work runs on
;;; a worker thread: the turn, the compactor, and later a subagent, which is
;;; just another worker that owns an agent struct of its own.
;;;
;;; Three rules keep this simple and race-free:
;;;
;;;   1. Only the UI thread writes to the terminal, the transcript entries, and
;;;      the chat. A worker that must do so calls UI-CALL, which runs a thunk on
;;;      the UI thread and returns its values; from the UI thread it just calls
;;;      it, so the one-shot and test paths behave as before.
;;;   2. Chat mutations take *CHAT-LOCK*, a recursive lock: the compactor and a
;;;      turn may both append to the chat. The lock covers short, leaf-level
;;;      updates only and is never held across a UI-CALL: the UI thread may
;;;      want it too, and that is the one ordering that deadlocks.
;;;   3. SBCL cannot fork or save an image while another thread runs, and a
;;;      checkpoint forks. The UI thread forks, after QUIESCE-WORKERS and a
;;;      graceful stop of the compactor; the compactor starts again after.

(defvar *ui-thread* nil
  "The thread that owns the terminal, which MAIN claims at startup. NIL means
no thread claims the terminal yet, and then every thread counts as the UI
thread: a one-shot run, or a test, uses no threads at all.")

(defun ui-thread-p ()
  "True on the UI thread, which may draw and mutate the chat directly."
  (or (null *ui-thread*) (eq (bt:current-thread) *ui-thread*)))

(defun claim-ui-thread ()
  "Make the calling thread the UI thread, as MAIN does at startup and as an
image does when it resumes."
  (setf *ui-thread* (bt:current-thread)))

(defun release-ui-thread ()
  (setf *ui-thread* nil))

;;; The UI queue: a worker posts a thunk, and the UI thread runs it as soon as
;;; it is back in its polling loop, between keystrokes.

(defvar *ui-queue* nil
  "Pending UI jobs, oldest first, guarded by *UI-QUEUE-LOCK*.")

(defvar *ui-queue-lock* (bt:make-lock "maxwell ui queue"))

(defstruct (ui-job (:constructor %make-ui-job))
  (function nil)
  ;; The worker waiting for this job, or NIL for posted work nobody waits on.
  (waiter nil)
  (lock (bt:make-lock "maxwell ui job"))
  (condition (bt:make-condition-variable :name "maxwell ui job"))
  (done nil)
  error
  (values '()))

(defun ui-post (function)
  "Queue FUNCTION for the UI thread and return at once, without waiting for
it. For work nobody waits on, such as a streamed reasoning trace update."
  (if (ui-thread-p)
      (progn (funcall function) t)
      (progn
        (bt:with-lock-held (*ui-queue-lock*)
          (setf *ui-queue* (nconc *ui-queue* (list (%make-ui-job :function function)))))
        t)))

(defun ui-call (function)
  "Run FUNCTION on the UI thread and return its values. On the UI thread it
runs at once; on a worker it is queued and this thread waits for the UI thread
to run it, then re-signals any error it signalled.

Two rules keep that wait short and safe. The UI thread drains the queue
whenever it is idle -- waiting for a key, or waiting for a turn -- and no
code holds the chat lock across a UI call. When the UI thread has gone, as in
a saved image that has resumed, the call runs where it is made rather than
waiting for a thread that will never answer."
  (if (ui-thread-p)
      (funcall function)
      (let ((job (%make-ui-job :function function :waiter (bt:current-thread))))
        (bt:with-lock-held (*ui-queue-lock*)
          (setf *ui-queue* (nconc *ui-queue* (list job))))
        (bt:with-lock-held ((ui-job-lock job))
          (loop until (or (ui-job-done job)
                          (and *ui-thread* (not (bt:thread-alive-p *ui-thread*))))
                do (bt:condition-wait (ui-job-condition job) (ui-job-lock job)))
          (unless (ui-job-done job)
            ;; Nobody will run this job: take it back and do it here.
            (bt:with-lock-held (*ui-queue-lock*)
              (setf *ui-queue* (delete job *ui-queue* :test #'eq)))
            (release-ui-thread)
            (return-from ui-call (funcall function))))
        (let ((values (ui-job-values job)))
          (if (ui-job-error job)
              (error (ui-job-error job))
              (values-list values))))))

(defun run-ui-job (job)
  "Run JOB's function on the UI thread and hand its outcome to the waiter.
A UI failure is re-signalled on the worker that asked for the job, where the
turn's own error handling sees it."
  (let* ((failure nil)
         (values (handler-case (multiple-value-list (funcall (ui-job-function job)))
                   (error (condition) (setf failure condition) nil))))
    (bt:with-lock-held ((ui-job-lock job))
      (setf (ui-job-error job) failure
            (ui-job-values job) values
            (ui-job-done job) t)
      (bt:condition-notify (ui-job-condition job)))
    (values-list values)))

(defun drain-ui-queue (&key (limit 64))
  "Run up to LIMIT queued UI jobs on the UI thread. Returns how many ran. A
job whose waiting worker has been cancelled and destroyed draws nothing: the
turn it belonged to is gone."
  (when (ui-thread-p)
    (let ((count 0))
      (loop while (< count limit)
            do (let ((job (bt:with-lock-held (*ui-queue-lock*)
                            (prog1 (first *ui-queue*)
                              (setf *ui-queue* (rest *ui-queue*))))))
                 (unless job (return))
                 (let ((waiter (ui-job-waiter job)))
                   (when (or (null waiter) (bt:thread-alive-p waiter))
                     (run-ui-job job)))
                 (incf count)))
      count)))

(defvar *ui-service* nil
  "When non-NIL, a function the UI thread runs whenever it is free: the
steering prompt's keystroke service, installed while a turn is in flight.")

(defun service-ui ()
  "What the UI thread does whenever it is free: run queued work, then service
keystrokes if a turn is in flight."
  (drain-ui-queue)
  (when *ui-service* (funcall *ui-service*)))

(defun pump-ui ()
  "Let the UI thread draw and read keys. On the UI thread this happens at
once; on a worker, the UI thread is asked and this thread waits for it."
  (if (ui-thread-p)
      (progn (service-ui) (sleep 0.02))
      (ui-call #'service-ui)))

(defun ui-yield (&optional (seconds 0.02))
  "Let the UI thread work: on a worker, sleep SECONDS so the UI thread can run
queued jobs; on the UI thread, run them instead."
  (if (ui-thread-p)
      (progn (drain-ui-queue) (sleep seconds))
      (sleep seconds)))

(defun ui-post-activity (state &optional detail)
  "Set the status bar's activity, on the UI thread when called from a worker:
the bar is drawn there."
  (if (ui-thread-p)
      (set-activity state detail)
      (ui-post (lambda () (set-activity state detail)))))

;;; Workers. Every thread Maxwell starts is registered, so that a checkpoint
;;; can stop them all.

(defvar *workers* '()
  "Live WORKERs, newest first, guarded by *WORKER-LOCK*.")

(defvar *worker-lock* (bt:make-lock "maxwell workers"))

(defstruct (worker (:constructor %make-worker))
  name
  thread)

(defun register-worker (worker)
  (bt:with-lock-held (*worker-lock*)
    (setf *workers* (cons worker *workers*)))
  worker)

(defun unregister-worker (worker)
  (bt:with-lock-held (*worker-lock*)
    (setf *workers* (remove worker *workers*)))
  worker)

(defun start-worker (name function &key (bindings '()))
  "Start FUNCTION on a new thread named NAME and return its WORKER. BINDINGS
is an alist of (SPECIAL . VALUE) bound around FUNCTION: a new thread inherits
no dynamic bindings, so whoever starts a worker hands it the state its work
depends on."
  (let* ((worker (%make-worker :name name))
         (gate (bt:make-semaphore :count 0)))
    (setf (worker-thread worker)
          (bt:make-thread
            (lambda ()
              (bt:wait-on-semaphore gate)
              (progv (mapcar #'car bindings) (mapcar #'cdr bindings)
                (unwind-protect (funcall function)
                  (unregister-worker worker))))
            :name name))
    (register-worker worker)
    (bt:signal-semaphore gate)
    worker))

(defun worker-alive-p (worker)
  (and worker (bt:thread-alive-p (worker-thread worker))))

(defun worker-join (worker)
  "Wait for WORKER to finish and return its value. On the UI thread, queued UI
work keeps running meanwhile: a worker that is unwinding, as a cancelled turn
does, may still ask the UI thread for something, and a plain join would then
deadlock with it."
  (when (and worker (ui-thread-p))
    (loop while (worker-alive-p worker)
          do (drain-ui-queue)
             (sleep 0.01)))
  (when (worker-alive-p worker)
    (bt:join-thread (worker-thread worker))))

(defun live-workers ()
  (bt:with-lock-held (*worker-lock*)
    (remove-if-not #'worker-alive-p *workers*)))

(defun quiesce-workers (&key (timeout 3) exclude)
  "Wait up to TIMEOUT seconds for the workers other than EXCLUDE to finish,
then stop the ones still running and say how many were stopped. A checkpoint
calls this: SBCL cannot fork or save an image while threads run."
  (let ((deadline (+ (get-internal-real-time)
                     (round (* timeout internal-time-units-per-second)))))
    (loop for remaining = (remove exclude (live-workers))
          while (and remaining (< (get-internal-real-time) deadline))
          do (ui-yield))
    (let ((stopped 0))
      (dolist (worker (remove exclude (live-workers)))
        (ignore-errors (bt:destroy-thread (worker-thread worker)))
        (ignore-errors (worker-join worker))
        (unregister-worker worker)
        (incf stopped))
      (values (null (remove exclude (live-workers))) stopped))))

;;; The chat lock. A turn's worker, the compactor, and the UI thread (a
;;; steering line) all append to the chat, so every mutation takes this lock.
;;; It is recursive because one mutation often calls another.

(defvar *chat-lock* (bt:make-recursive-lock "maxwell chat"))

(defmacro with-chat-lock (&body body)
  `(bt:with-recursive-lock-held (*chat-lock*) ,@body))

;;; Lock order: the chat lock covers short, leaf-level updates only, and is never
;;; held while waiting on the UI thread. A worker records a transcript entry and
;;; asks the UI thread for work outside it. Holding it across such a wait
;;; deadlocks as soon as the UI thread wants the chat lock too.

;;; The compactor's worker. Summaries are built by provider requests that run as
;;; child processes; the worker only starts them and reaps them, which used to
;;; happen between keystrokes on the one thread.

(defvar *compactor-worker* nil
  "The worker that advances background compaction, or NIL when it is stopped.")

(defvar *compactor-tick* nil
  "The function the compactor worker calls, kept so that a checkpoint can start
the worker again after it has stopped it.")

(defvar *compactor-run* nil
  "True while the compactor worker should keep ticking. Stopping the worker clears
it, so the loop ends by itself instead of being destroyed in the middle of a tick
that a provider request is running.")

(defparameter *compactor-interval* 0.15
  "Seconds between the compactor worker's ticks.")

(defun compactor-worker-live-p ()
  (worker-alive-p *compactor-worker*))

(defun compactor-worker-main (tick)
  ;; STOP-COMPACTOR-WORKER clears *COMPACTOR-RUN* on this thread, so the loop
  ;; ends at its next test rather than in the middle of a tick.
  (loop while *compactor-run*
        do (funcall tick)
           (sleep *compactor-interval*)))

(defun start-compactor-worker (tick)
  "Start the compactor on a worker of its own, calling TICK until the worker is
stopped. Does nothing when it already runs. A checkpoint stops it and starts it
again this way."
  (setf *compactor-tick* tick)
  (unless (compactor-worker-live-p)
    (setf *compactor-worker*
          (start-worker "maxwell compactor"
                        (lambda () (compactor-worker-main tick))
                        :bindings (list (cons '*compactor-run* t)))))
  *compactor-worker*)

(defun restart-compactor-worker ()
  "Start the compactor worker again after a checkpoint stopped it. Does nothing
when no compactor ever ran, as in a one-shot run."
  (when *compactor-tick* (start-compactor-worker *compactor-tick*)))

(defun stop-compactor-worker (&optional (timeout 3))
  "Stop the compactor worker and wait for it to end. A checkpoint does this first:
SBCL cannot fork or save an image while threads run. The thread is unregistered
first, so QUIESCE-WORKERS does not wait for it again."
  (let ((worker *compactor-worker*))
    (setf *compactor-worker* nil)
    (when (worker-alive-p worker)
      (unregister-worker worker)
      ;; Ask the loop to end, and give the tick it is in a moment to finish.
      (ignore-errors
        (bt:interrupt-thread (worker-thread worker)
                             (lambda () (setf *compactor-run* nil))))
      (let ((deadline (+ (get-internal-real-time)
                         (round (* timeout internal-time-units-per-second)))))
        (loop while (and (worker-alive-p worker)
                         (< (get-internal-real-time) deadline))
              do (ui-yield 0.02)))
      ;; A tick that will not end, such as a slow provider request, cannot survive
      ;; the fork, so it is stopped outright.
      (when (worker-alive-p worker)
        (ignore-errors (bt:destroy-thread (worker-thread worker)))
        (ignore-errors (worker-join worker)))))
  t)
;;; Interrupts. Ctrl-C arrives as SIGINT, which SBCL would turn into an
;;; interactive interrupt on whichever thread it chose. Maxwell handles it
;;; itself instead: the handler only raises a flag, the UI thread acts on it,
;;; and a turn in flight is cancelled. That keeps one rule: the UI thread
;;; decides.

(defvar *interrupt-requested* nil
  "True once Ctrl-C has been pressed and not yet acted on.")

(defvar *interrupt-handler-installed* nil)

(defun request-interrupt ()
  (setf *interrupt-requested* t))

(defun take-interrupt ()
  "True when a Ctrl-C is pending, clearing it."
  (prog1 *interrupt-requested* (setf *interrupt-requested* nil)))

(defun install-interrupt-handler ()
  "Handle SIGINT as Ctrl-C: cancel the turn in flight, or drop the line being
typed. Idempotent."
  (unless *interrupt-handler-installed*
    (setf *interrupt-handler-installed* t)
    #+sbcl
    (sb-sys:enable-interrupt sb-unix:sigint
                             (lambda (signal code context)
                               (declare (ignore signal code context))
                               (request-interrupt))))
  t)






















