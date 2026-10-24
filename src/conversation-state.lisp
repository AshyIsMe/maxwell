(in-package #:maxwell)

(defstruct (conversation (:constructor %make-conversation))
  id name parent agent chat
  (entries (make-array 16 :adjustable t :fill-pointer 0))
  (inbox nil) (steering nil) turn
  (state :idle) report-to
  (draft ""))

(defstruct conversation-note id from text (wake nil) delivered)
(defvar *conversations* nil)
(defvar *next-conversation-id* 0)
(defvar *next-note-id* 0)
(defvar *conversation-lock* (bt:make-recursive-lock "maxwell conversations"))

