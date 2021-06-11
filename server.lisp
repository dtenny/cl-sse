;;;;
;;;; Support for sending SSE events.
;;;;
;;;; Realistically, if you are familar with SSE protocols you might just do
;;;; `(format out "event: my-event~%data: my-event-data~%~%")`
;;;; and be done with it.  However if you are not familiar with the spec and if your data
;;;; is a bit more complicated, then `send-event!` will arrange to send the inputs
;;;; with the correct structure.
;;;;
;;;; You must have a UTF-8 output stream.
;;;; The stream must be packaged in a `text/event-stream` Content-Type.
;;;;

(defpackage :sse-server
  (:use :cl)
  (:export :send-comment!
           :send-event!)
  (:documentation
   "Functions to send SSE event data to a stream. 
`send-event!` sends a fully formed and blank-line terminated event.
`send-comment` sends a comment, and may be generally useful before sending an event, or 
for debugging"))

(in-package :sse-server)
(named-readtables:in-readtable trivial-escapes:readtable) ;c-style escapes, e.g. '\n'

(defun string->sse-data (str)
  "Given a string containing logical newlines, generate a list of substrings for each line.
Note that empty lines are not returned, e.g.:
   #\"abc\\n\", \"abc\" result in one line (note #\" readtable dispatch for c-style escapes).
   #\"\\n\", and \"\" result zero lines.
   #\"\\n\\n\" results in zero lines.

CR, LF, and CRLF are treated as line separators."
  (cl-ppcre:split #"(?:\r\n)|\r|\n" str))

(defun send-field! (ostream field-name str)
  "Write data to ostream as lines of the form 'field-name: str'.
  If str contains newlines, split it into pieces and write each line as a 'field-name: str'."
  (declare (string field-name) (string str))
  (mapc (lambda (line)
          (write-string field-name ostream)
          (write-string ": " ostream)   ;space necessary after colon for fields
          (write-line line ostream))
        (string->sse-data str)))

(defun send-comment! (ostream str)
  "Write string to the output stream as an SSE comment (which start with ':').
If the string embeds newlines, send it as multiple comment lines."
  (declare (string str))
  (mapc (lambda (line) 
          (write-char #\: ostream)      ;no space necessary after colon for comments
          (write-line line ostream))
        (string->sse-data str)))

(defun send-event! (ostream event data &key id retry fields)
  "Write event data to an output stream  according to the SSE protocol requirements.

Arguments:
  ostream - An output stream to which the event will be written.

  ;; Specially defined EventSource field names (per the spec)
  ;; These values are sent with field names matching the parameter name.
  event   - String naming the event-type. 
            Note that this has nothing to do with lisp types.
            TBD: The spec will allow an event to consist only of data
            so not clear this is in our best interest requiring an event (type) property.
  data    - String data to be sent.
  id      - Optional integer/string sequence value for client lastEventID use.
  retry   - Optional integer/string specifying millisconds for the event stream's reconnect time.
            Frankly I'm not sure about how this works other than it must be taken by the client
            and will be ignored if it can't be parsed as an integer.

  ;; Optional/alternative user defined field names
  fields  - Optional alist of optional fields to be written as part of the event.
            Alist keys represent the (string) field names, and values should be strings.
            It is probably an error to have a field nameed 'event-type'.

No values should end in newlines, those are handled
by this function (but a :data value may embedded newlines).

Return value N/A. Signal error if there are any problems writing to ostream."

  (declare (string event) (string data)
           ((or string null integer) id) ((or string null integer) retry))
  (send-field! ostream "event" event)
  (when id
    (send-field! ostream "id" (princ-to-string id)))
  (send-field! ostream "data" data)
  (when retry
    (send-field! ostream "retry" (princ-to-string retry)))
  (loop for (k . v) in fields do (send-field! ostream k v))
  (terpri ostream))                      ;blank line signals end of event
