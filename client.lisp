
;;;; `read-event` will read Server Sent Events off an input stream, one at a time
;;;; in a manner similar to how the javascript EventSource implementation does in a browser.
;;;; However reading the event is where we stop, there is presently no attempt to emulate
;;;; EventSource behavior (as there is no perceived need).
;;;;
;;;; This module exists to help Common Lisp services (rather than browser clients)
;;;; can read SSE events.  It can be useful if you're looking for a simple unidirectional
;;;; protocol for uses like job dispatch (workers get an SSE connection and wait for
;;;; events representing dispatch requests). 

(defpackage :sse-client
  (:use :cl)
  (:export 
   :event
   :event-p
   :make-event
   :copy-event
   :event-event
   :event-data
   :event-id
   :event-retry

   :read-event)
  (:documentation
   "Tools to read and represent EventSource data according to
https://www.w3.org/TR/eventsource/#event-stream-interpretation.

This module implements section 7 up to but not including the paragraph beginning with
\"When the user agent is required to dispatch the event\".
So we have executed the \"steps to process the field\".   Notably:
1. data values will have a trailing newline.
2. field names other than 'event', 'data', 'id', and 'retry' will be discarded.
3. 'retry' will be converted to an integer if it conforms, or will be nil otherwise.

The stream-leading byte order mark, if present, must be handled by the caller, 
as one time stream setup logic. (Ignore the leading U+FEFF BYTE ORDER MARK if present.)"
   ))

(in-package :sse-client)

(defstruct (event (:constructor make-event (event data id retry)))
  "Representation of an SSE event having completed the steps 'processing the field'.
Slots are named according the the incoming field names in the stream, not the buffer names."
  (event "" :type string) ;the event type buffer
  (data  "" :type string) ;the data buffer, every event must have a data: line
  (id    "" :type string) ;the last-event-ID buffer
  (retry nil :type (or integer null))) ;nil if it should be ignored for non-numeric content

(defun make-event-from-fields (fields)
  "Given an association list of event fields, 
e.g. '((\"data\" . \"123\") (\"event\" . \"timer\"))
Create and return an event instance with the data."
  (flet ((fget (key) (or (cdr (assoc key fields :test #'string=)) "")))
    (make-event 
     (fget "event")
     (concatenate 'string (fget "data") #(#\newline))
     (fget "id")
     (handler-case (parse-integer (fget "retry"))
       (parse-error () nil)))))

(defun comment-line-p (line)
  "Return non-nil if the line is an SSE comment line, nil otherwise."
  (declare (string line))
  (and (> (length line) 0)
       (eql (char line 0) #\:)))

(defun parse-field-name (line)
  "Examine the input line, a string, already known not to be a comment line.
By definition it must contain a field name, return the field name.
It may or may not have a colon and field value."
  (declare (string line))
  (let ((pos (position #\: line)))
    (if pos
        (subseq line 0 pos)
        line)))

(defun parse-value (line)
  "Examine the input line, a string, already known not to be a comment line.
If it contains a field name only, return an empty string.
If it returns field value, return the value.
Note that per the spec, a space following the colon, if any, is discarded."
  (declare (string line))
  (let* ((pos (position #\: line))
         (space? (and pos 
                      (< pos (1- (length line)))
                      (eql #\space (char line (1+ pos))))))
    (if pos
        (subseq line (if space? 
                         (+ 2 pos)
                         (+ 1 pos)))
        "")))                           ;whole line is the field name

(defun stash-line (line fields)
  "Stash the string 'line', which is known not to be empty or a comment line,
into the association list 'fields', with the field name in the car, and the
field value (if any) in the cdr.  E.g. (\"data\" . \"abc\") 

No ':' is stored with the field name.

If the 'fields' association list already contains an entry for the car/key, 
new line values for the field will be appended to prior values, so multiple `data:` lines
will be appended.

The fields argument may or may not be destructively modified, be sure to update
the caller's pointer to head of list with the return value."
  (declare (string line))
  (let* ((field-name (parse-field-name line)) ;required
         (value (parse-value line))          ;optional
         (existing-cons (assoc field-name fields :test #'string=)))
    (if existing-cons 
        ;; Destructive update to existing list
        (progn (rplacd existing-cons (concatenate 'string (cdr existing-cons) #(#\newline) value))
               fields)
        ;; Prepend new element to list.
        (acons field-name value fields))))

(defun has-data-p (fields)
  "Return non-nil if the association list `fields` has a field field named 'data',
otherwise return nil."
  (assoc "data" fields :test #'string=))

;; TODO: put this in a standalone toolkit, I know I'll want it again.
(defun read-line+ (stream &optional (eof-error-p t) eof-value recursive-p)
  "Like `read-line`, except that we accept CR, CRLF, and LF as line
terminators instead of just LF. Inefficient, we're allocating two arrays
(general purpose, and result string) per line read, failing to use optimized
vector calls, and so on."
  (declare (ignore recursive-p))
  (let ((cr-pending? nil)
        (buffer (make-array 80 :element-type 'base-char :adjustable t :fill-pointer 0)))
    (flet ((line ()                     ;line finished
             (let ((s (make-string (length buffer))))
               (replace s buffer)
               (setf (fill-pointer buffer) 0)
               (setq cr-pending? nil)
               s)))
      (loop
         (let ((ch (read-char stream nil nil)))
           (cond (ch                      ;not EOF
                  (cond ((char= ch #\newline) ;CRLF or LF
                         (return (values (line) nil)))
                        ((char= ch #\return) ;CR - don't know for sure we have a line yet
                         (if cr-pending?     ;we have CRCR, cut a line up through first cr
                              (progn
                               (unread-char ch stream) ;put the second CR back, we're done
                               (return (values (line) nil)))
                             (setq cr-pending? t))) ;first CR
                        (t (if cr-pending? ;Not CR/LF, So pending CR terminated line
                               (progn
                                 (unread-char ch stream) ;put back first char of next line
                                 (return (values (line) nil)))
                               (vector-push-extend ch buffer)))))

                 ;; No char, so we're at EOF
                 ((> (length buffer) 0) (return (values (line) t))) ; last line terminated by EOF
                 (eof-error-p (error 'end-of-file :stream stream))
                 (t (return (values eof-value t)))))))))

(defun read-event (input-stream)
  "Read an event from the character input stream and return it if successful.

An event is basically a sequence of consecutive non-empty lines with various
field keys and possibly values, followed by a blank line.

Any field whose name appears on consecutive lines has the value portions appended.
This is certainly within the spec for `data` values, I'm less certain about those semantics
for other field types (which is most likely with non-standard user defined field types).

Per the EventSource spec, events lacking `data` fields are invalid.
Such events are discarded by this function as if they were comments.
Note that a `data:` line with no value is valid however.

Return nil if we hit EOF."

  ;; Note that loop returns nil unless you have accumulators or a return construct of some form.
  (loop
     with fields = nil
     for  line = (read-line+ input-stream nil)
     as   not-empty-line? = (> (length line) 0)
     while (if fields
               not-empty-line?
               line)                    ;not nil
     when (and not-empty-line? (not (comment-line-p line)))
     do (setq fields (stash-line line fields))
     finally (when (has-data-p fields)
               (return (make-event-from-fields fields)))))

;;; *FINISH* check read-line crlf processing?

