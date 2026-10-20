(in-package #:maxwell)

;;; Per-request statistics. Every model request a turn makes is measured: the
;;; time to the first streamed piece, the total response time, and the token
;;; counts the provider reports in its usage -- input, output, and the reasoning
;;; part of the output. When a provider reports no usage, the output tokens are
;;; estimated from the reply's size and marked as such. *REQUEST-STATS* keeps
;;; the newest records, so /stats can show recent requests and their averages;
;;; the records live in the global image like the chat.

(defparameter *request-stats-limit* 200
  "The most request records kept in *REQUEST-STATS*.")

(defvar *request-stats* '()
  "Newest-first list of REQUEST-STAT records, one per measured model request.")

(defstruct request-stat
  model
  wire-api
  started          ; universal time of the request
  (real-start 0)   ; internal real time at the start
  first-real       ; internal real time of the first streamed piece
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
  (make-request-stat :model model :wire-api wire-api :started time
                     :real-start (get-internal-real-time)))

(defun note-request-piece (stat size)
  "Record a streamed piece of SIZE bytes for STAT, keeping the time of the
first non-empty one as the request's latency."
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
      (push stat *request-stats*)
      (when (> (length *request-stats*) *request-stats-limit*)
        (setf *request-stats* (subseq *request-stats* 0 *request-stats-limit*))))
    stat))

(defun latest-request-stat ()
  "The most recent request record, or NIL."
  (first *request-stats*))

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
  (format nil "~A  ~@[first token ~,2Fs, ~]total ~,2Fs~@[  ~,1F tok/s~]~@[  ~:D in +~]~@[ ~:D out~]~@[ (incl. ~:D reasoning)~]~@[  = ~:D tokens~]~:[~;  (estimated)~]"
          (request-stat-model-name stat)
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

(defun request-stats-report (&optional (count 5))
  "A multi-line report of the last COUNT requests and their averages."
  (let ((stats *request-stats*))
    (if (null stats)
        "No requests recorded yet."
        (let ((shown (subseq stats 0 (min count (length stats))))
              (latencies (remove nil (mapcar #'request-stat-latency stats)))
              (rates (remove nil (mapcar #'request-stat-tokens-per-second stats)))
              (tokens (remove nil (mapcar #'request-stat-total-tokens stats))))
          (with-output-to-string (out)
            (format out "Last ~D request~:P (newest first):" (length shown))
            (dolist (stat shown)
              (format out "~&  ~A" (request-stat-line stat)))
            (format out "~&Average over ~D request~:P:~@[ first token ~,2Fs,~]~@[ ~,1F tok/s,~]~@[ ~:D tokens~]"
                    (length stats)
                    (average-value latencies)
                    (average-value rates)
                    (let ((average (average-value tokens)))
                      (and average (round average)))))))))
