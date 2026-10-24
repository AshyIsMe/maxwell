(in-package #:maxwell)

;;; Per-request statistics. Every model request a turn makes is measured: the
;;; time to the first generated output, the total response time, and the token
;;; counts the provider reports in its usage -- input, output, and the reasoning
;;; part of the output. When a provider reports no usage, the output tokens are
;;; estimated from the reply's size and marked as such. *REQUEST-STATS* keeps
;;; the newest records for the retention time, so /stats can show recent
;;; requests and their summary; the records live in the global image like the
;;; chat.

(defparameter *request-stats-retention-days*
  (or (ignore-errors
        (let ((days (uiop:getenv "MAXWELL_STATS_DAYS")))
          (and (stringp days) (plusp (length days))
               (parse-integer days :junk-allowed t))))
      30)
  "How many days of request records *REQUEST-STATS* keeps. Set from
MAXWELL_STATS_DAYS, otherwise 30.")

(defparameter *request-stats-limit* 20000
  "The most request records kept in *REQUEST-STATS*: a hard ceiling on top of
the retention time, so an extremely busy month cannot grow without bound.")

(defvar *request-stats-lock* (bt:make-recursive-lock "maxwell stats"))

(defvar *request-stats* '()
  "Newest-first list of REQUEST-STAT records, one per measured model request.")

(defstruct request-stat
  conversation-id
  model
  wire-api
  started          ; universal time of the request
  (real-start 0)   ; internal real time at the start
  first-real       ; internal real time of the first generated output
  (latency nil)    ; seconds to the first piece, when it was measured
  (total nil)      ; seconds for the whole reply
  prompt-tokens
  completion-tokens
  reasoning-tokens
  total-tokens
  (tokens-per-second nil)
  (estimated nil))

(defun request-stat-seconds (ticks)
  (/ ticks internal-time-units-per-second))

(defun request-stat-begin (model wire-api &key (time (get-universal-time)))
  "Start measuring a request to WIRE-API for MODEL."
  (make-request-stat :conversation-id (and *conversation* (conversation-id *conversation*))
                     :model model :wire-api wire-api :started time
                     :real-start (get-internal-real-time)))

(defun note-request-piece (stat size)
  "Record a streamed piece of SIZE bytes for STAT, keeping the time of the
first non-empty generated piece as the request's latency."
  (when (and stat (null (request-stat-first-real stat)) (plusp size))
    (setf (request-stat-first-real stat) (get-internal-real-time)))
  stat)

;;; Token accounting

(defun request-stat-integer (object keys)
  "The first integer value of OBJECT under one of KEYS, or NIL."
  (loop for key in keys
        for value = (get-key object key)
        when (integerp value) return value))

(defun usage-token-counts (usage)
  "Prompt, completion, total, and reasoning tokens from USAGE, an OpenAI or
Responses style usage object, as a list."
  (when (hash-table-p usage)
    (let ((details (or (get-key usage "completion_tokens_details")
                       (get-key usage "output_tokens_details"))))
      (list (request-stat-integer usage '("prompt_tokens" "input_tokens"))
            (request-stat-integer usage '("completion_tokens" "output_tokens"))
            (request-stat-integer usage '("total_tokens"))
            (and (hash-table-p details)
                 (request-stat-integer details '("reasoning_tokens")))))))

(defun response-reply-message (response)
  "The assistant message of RESPONSE in Chat Completions form, or NIL."
  (let ((choices (get-key response "choices")))
    (and (vectorp choices) (plusp (length choices))
         (get-key (aref choices 0) "message"))))

(defun response-output-size (response)
  "The characters of visible output and reasoning in RESPONSE, for estimating
tokens when the provider reported no usage."
  (let ((message (response-reply-message response)))
    (if (hash-table-p message)
        (+ (let ((text (get-key message "content")))
             (length (if (stringp text) text "")))
           ;; The reasoning key mirrors PROVIDER's +REASONING-KEY+; it is
           ;; written out here so this file never depends on a later one.
           (length (or (get-key message "_maxwell_reasoning") "")))
        0)))

(defun request-stat-take-usage (stat usage)
  "Fill STAT's token counts from USAGE. True when USAGE was a usage object."
  (when (hash-table-p usage)
    (destructuring-bind (prompt completion total reasoning) (usage-token-counts usage)
      (setf (request-stat-prompt-tokens stat) prompt
            (request-stat-completion-tokens stat) completion
            (request-stat-total-tokens stat) total
            (request-stat-reasoning-tokens stat) reasoning))
    t))

(defun request-stat-finish-tokens (stat)
  "Fill in the totals and the output tokens per second from whatever is known."
  (when (and (request-stat-completion-tokens stat)
             (null (request-stat-total-tokens stat)))
    (setf (request-stat-total-tokens stat)
          (+ (or (request-stat-prompt-tokens stat) 0)
             (request-stat-completion-tokens stat))))
  (let ((duration (if (request-stat-latency stat)
                      (- (request-stat-total stat) (request-stat-latency stat))
                      (request-stat-total stat))))
    ;; A stream that ends the moment it starts can leave LATENCY at zero; the
    ;; whole response time is then the honest denominator.
    (when (and duration (not (plusp duration)))
      (setf duration (request-stat-total stat)))
    (when (and (request-stat-completion-tokens stat) duration (plusp duration))
      (setf (request-stat-tokens-per-second stat)
            (/ (request-stat-completion-tokens stat) duration))))
  stat)

(defun finish-request-stat (stat response)
  "Finish measuring STAT for RESPONSE, record it, and return it."
  (when stat
    (let ((now (get-internal-real-time)))
      (when (and (request-stat-first-real stat)
                 (null (request-stat-latency stat)))
        (setf (request-stat-latency stat)
              (request-stat-seconds
                (- (request-stat-first-real stat) (request-stat-real-start stat)))))
      (setf (request-stat-total stat)
            (request-stat-seconds (- now (request-stat-real-start stat))))
      (unless (and (hash-table-p response)
                   (request-stat-take-usage stat (get-key response "usage")))
        (let ((size (response-output-size response)))
          (when (plusp size)
            (setf (request-stat-completion-tokens stat) (ceiling size 4)
                  (request-stat-estimated stat) t))))
      (request-stat-finish-tokens stat)
      (bt:with-recursive-lock-held (*request-stats-lock*)
        (push stat *request-stats*)
        (expire-request-stats)))
    stat))

(defun request-stats-cutoff (&key (time (get-universal-time)))
  "The universal time before which a request record is too old to keep."
  (- time (* *request-stats-retention-days* 24 60 60)))

(defun expire-request-stats (&key (time (get-universal-time)))
  "Drop records older than the retention time, then any beyond the ceiling.
*REQUEST-STATS* is newest first, so old records sit at its end."
  (bt:with-recursive-lock-held (*request-stats-lock*)
  (let ((cutoff (request-stats-cutoff :time time)))
    (setf *request-stats*
          (remove-if (lambda (stat)
                       (let ((started (request-stat-started stat)))
                         (and started (< started cutoff))))
                     *request-stats*)))
  (when (> (length *request-stats*) *request-stats-limit*)
    (setf *request-stats* (subseq *request-stats* 0 *request-stats-limit*)))
  *request-stats*))

(defun latest-request-stat ()
  "The most recent request record, or NIL."
  (bt:with-recursive-lock-held (*request-stats-lock*)
    (if *conversation*
        (find (conversation-id *conversation*) *request-stats* :key #'request-stat-conversation-id)
        (first *request-stats*))))

;;; Reporting

(defun request-stat-model-name (stat)
  (let ((model (request-stat-model stat)))
    (if (and model (plusp (length model))) model "(no model)")))

(defun request-stat-reasoning-count (stat)
  "STAT's reasoning tokens when any were reported, else NIL, so a zero is
never shown."
  (let ((tokens (request-stat-reasoning-tokens stat)))
    (and tokens (plusp tokens) tokens)))

(defun request-stat-line (stat)
  "One line describing STAT."
  (format nil "~A~@[ [conversation ~D]~]  ~@[first token ~,2Fs, ~]total ~,2Fs~@[  ~,1F tok/s~]~@[  ~:D in +~]~@[ ~:D out~]~@[ (incl. ~:D reasoning)~]~@[  = ~:D tokens~]~:[~;  (estimated)~]"
          (request-stat-model-name stat)
          (request-stat-conversation-id stat)
          (request-stat-latency stat)
          (request-stat-total stat)
          (request-stat-tokens-per-second stat)
          (request-stat-prompt-tokens stat)
          (request-stat-completion-tokens stat)
          (request-stat-reasoning-count stat)
          (request-stat-total-tokens stat)
          (request-stat-estimated stat)))

(defun average-value (numbers)
  (when numbers (/ (reduce #'+ numbers) (length numbers))))

(defun request-stat-answer-seconds (stat)
  "The seconds STAT spent answering, past its first streamed piece: the
denominator of its output rate. A reply that ended the moment it started falls
back to its whole response time."
  (let ((total (request-stat-total stat))
        (latency (request-stat-latency stat)))
    (when total
      (let ((duration (if latency (- total latency) total)))
        (if (plusp duration) duration total)))))

(defun request-stats-summary (stats)
  "A one-line summary over STATS. The output rate is tokens weighted: the
completion tokens of every request over the answering time of every request,
so a slow request with many tokens counts for more than its share of the mean
of the per-request rates."
  (let ((latencies (remove nil (mapcar #'request-stat-latency stats)))
        (tokens (remove nil (mapcar #'request-stat-total-tokens stats)))
        (completion 0)
        (answering 0))
    (dolist (stat stats)
      (let ((count (request-stat-completion-tokens stat))
            (seconds (request-stat-answer-seconds stat)))
        (when (and count seconds (plusp seconds))
          (incf completion count)
          (incf answering seconds))))
    (format nil "~D request~:P (last ~D days),~@[ first token ~,2Fs on average,~]~@[ ~:D tokens in all,~] ~:[no tokens measured~;~,1F tok/s over answering time~]"
            (length stats)
            *request-stats-retention-days*
            (average-value latencies)
            (and tokens (round (reduce #'+ tokens)))
            (plusp answering)
            (if (plusp answering) (/ completion answering) 0))))

(defun request-stats-report (&optional (count 5))
  "A multi-line report of the last COUNT requests and their averages."
  (expire-request-stats)
  (let ((stats *request-stats*))
    (if (null stats)
        "No requests recorded yet."
        (let ((shown (subseq stats 0 (min count (length stats)))))
          (with-output-to-string (out)
            (format out "Last ~D request~:P (newest first):" (length shown))
            (dolist (stat shown)
              (format out "~&  ~A" (request-stat-line stat)))
            (format out "~&Summary: ~A" (request-stats-summary stats)))))))
