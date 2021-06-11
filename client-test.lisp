(defpackage :sse-client-test
  (:use :cl :fiveam :sse-client)
  (:import-from :sse-client #:read-line+))

(in-package :sse-client-test)

(named-readtables:in-readtable trivial-escapes:readtable) ;c-style escapes, e.g. '\n'

(def-suite test-suite :description "SSE client tests")
(in-suite test-suite)

(test test-read-line+
  "Test read-line+"
  (with-input-from-string (in #"abc\rdef\nghi\r\njkl\n")
    (is (equalp '("abc" nil) (multiple-value-list (read-line+ in nil nil))))
    (is (equalp '("def" nil) (multiple-value-list (read-line+ in nil nil))))
    (is (equalp '("ghi" nil) (multiple-value-list (read-line+ in))))
    (is (equalp '("jkl" nil) (multiple-value-list (read-line+ in))))
    (is (equalp '(nil t) (multiple-value-list (read-line+ in nil))))
    (is (equalp '(eof t) (multiple-value-list (read-line+ in nil 'eof))))
    (is (eql t (handler-case (progn (read-line+ in) nil)
                 (end-of-file () t)))))

  (let ((results '(("" nil) ("a" nil) ("" nil) ("b" t))))
    ;; Test my expectations 
    (with-input-from-string (in #"\na\n\nb")
      (dolist (result results)
        (is (equalp result (multiple-value-list (read-line+ in nil))))))
    ;; Compare to stock read-line
    (with-input-from-string (in #"\na\n\nb")
      (dolist (result results)
        (is (equalp result (multiple-value-list (read-line in nil))))))
    ;; Make sure other termination sequences perform the same way
    (with-input-from-string (in #"\ra\r\rb")
      (dolist (result results)
        (is (equalp result (multiple-value-list (read-line+ in nil))))))
    (with-input-from-string (in #"\r\na\r\n\r\nb")
      (dolist (result results)
        (is (equalp result (multiple-value-list (read-line+ in nil))))))
    ))

(test test-read-event
  "Test read-event"
  (with-input-from-string (in #"data:abc\n\n")
    (let ((event (read-event in)))
      (is (event-p event))
      (is (string= #"abc\n" (event-data event)))
      (is (string= "" (event-event event)))
      (is (string= "" (event-id event)))
      (is (null (event-retry event))))
    (is (null (read-event in))))

  (with-input-from-string (in #"data: abc\ndata: def\nevent: my-event\nretry: 23\nid: 1\n\n")
    (let ((event (read-event in)))
      (is (event-p event))
      (is (string= #"abc\ndef\n" (event-data event)))
      (is (string= "my-event" (event-event event)))
      (is (eql 23 (event-retry event)))
      (is (string= "1" (event-id event))))
    (is (null (read-event in))))

  (with-input-from-string (in #"\n\ndata: abc\n\n\n\ndata: def\n")
    (let ((event (read-event in)))
      (is (event-p event))
      (is (string= #"abc\n" (event-data event))))
    (let ((event (read-event in)))
      (is (event-p event))
      (is (string= #"def\n" (event-data event))))
    (is (null (read-event in))))

  (with-input-from-string (in #"data: abc\r\r")
    (let ((event (read-event in)))
      (is (string= #"abc\n" (event-data event))))
    (is (null (read-event in)))))

(explain! (run 'test-suite))
