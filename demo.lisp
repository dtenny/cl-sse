;;;;
;;;; Fire up a web service, access it with an EventSource enabled browser 
;;;; (basically a non-microsoft browser).
;;;;
;;;; 1. (ql:quickload :sse-demo)
;;;; 2. (sse-demo:start-server)
;;;; 3. Connect browser to localhost:8080/sse
;;;;

(defpackage :sse-demo
  (:use :cl)
  (:export :start-server :stop-server)
  (:documentation
   "Serves up a web page that will emit events to an 
EventSource enabled browser (which is most browsers that aren't provided by Microsoft)."))

(in-package :sse-demo)

(defvar *http-server* nil)

(defun start-server (&key (port 8080))
  "Start the web service listening on the indicated port."
  (setq *http-server* (hunchentoot:start (make-instance 'easy-routes:routes-acceptor
                                                       :port port)))
  (format t "SSE demo at localhost:8080/sse~%"))

(defun stop-server ()
  (when *http-server*
    (hunchentoot:stop *http-server*)
    (setq *http-server* nil)))

(easy-routes:defroute root ("/") ()
  (format nil "Hello World
Try the <a>/sse</a> endpoint for the demo."))

(easy-routes:defroute sse ("/sse") ()
  
  ;; With thanks to https://gist.github.com/jareware/aae9748a1873ef8a91e5
  ;; This will run some javascript code in the browser, which will in turn ask for the
  ;; the /events URL.

"<!DOCTYPE html>
<html>
<head>
    <meta charset=\"utf-8\" />
    <meta name=\"viewport\" content=\"width=device-width, height=device-height\" />
    <title>Server-Sent Events Demo</title>
    <style type=\"text/css\">
        body {
            font-family: 'Open Sans', sans-serif;
        }
    </style>
</head>
<body>

    <h1>Server-Sent Events Demo</h1>

    <ul></ul>

    <script>
        (function() { \"use strict\";
            var ul = document.querySelector('ul');
            var es = new EventSource('/events');
            function li(text) {
                var li = document.createElement('li');
                li.innerText = text;
                ul.appendChild(li);
            }
            es.addEventListener('open', function() {
                li('Server connected :)');
            });
            es.addEventListener('my-custom-event', function(event) {
                li(event.data);
            });
            es.addEventListener('error', function() {
                li('Server unavailable :(');
            });
        })();
    </script>

</body>
</html>
")

(defvar *utf-8* (flex:make-external-format :utf-8 :eol-style :lf))

(easy-routes:defroute events ("/events") ()
  ;; See `stream-direct` and `stream-direct-utf-8` in hunchentoot/test/test-handler.lisp
  ;; for examples with flexi-streams and such.
  (setf (hunchentoot:content-type*) "text/event-stream; charset=utf-8")
  (setf (hunchentoot:reply-external-format*) *utf-8*)
  (hunchentoot:no-cache)
  (let ((counter 0)
        (output-stream (flex:make-flexi-stream (hunchentoot:send-headers)
                                               :external-format *utf-8*)))
    (loop repeat 10
         do 
         (sleep 2)
         ;; Without sse-server module
         ;;(format output-stream "event:my-custom-event~%data:Hello World! ~d~%~%" (incf counter))
         ;; With sse-server module
         (sse-server:send-event! output-stream 
                                 "my-custom-event"
                                 (format nil "Hello World! ~d" (incf counter)))
         (finish-output output-stream))))
