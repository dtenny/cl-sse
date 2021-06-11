(in-package :cl-user)

(defpackage :cl-sse-client
  (:use :cl :asdf))

(in-package :cl-sse-client)

(defsystem :sse-client
  :version "0.1.0"
  :author "Dave Tenny"
  :description "Implements client parsing of a Server Sent Events (SSE) stream."
  :components ((:file "client")))
  
