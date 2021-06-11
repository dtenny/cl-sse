(in-package :cl-user)

(defpackage :sse-demo-asd
  (:use :cl :asdf))

(in-package :sse-demo-asd)

(defsystem :sse-demo
  :version "0.1.0"
  :author "Dave Tenny"
  :description "Use sse-server + a web service to serve SSE events to a browser."
  :depends-on (:sse-server :hunchentoot :easy-routes :flexi-streams :sse-server)
  :components ((:file "demo")))
  
