(defpackage :sse-server-test
  (:use :cl :fiveam :sse-server))

(in-package :sse-server-test)

(named-readtables:in-readtable trivial-escapes:readtable) ;c-style escapes, e.g. '\n'

(def-suite test-suite :description "SSE server tests")
(in-suite test-suite)

(test test-comment
  "Test send-comment!"
  (is (string= #":This is a comment\n"
               (with-output-to-string (out)
                 (send-comment! out "This is a comment")
                 ))))

(test test-event
  "Test send-event!"
  (flet ((se (&rest args)
           (with-output-to-string (out)
             (apply #'send-event! out args))))
    (is (string= #"event: myevent\ndata: abc\n\n"
                 (se "myevent" "abc")))
    (is (string= #"event: x\nid: 1\ndata: y\nretry: 23\n\n"
                 (se "x" "y" :id 1 :retry 23)))))

(explain! (run 'test-suite))
