(in-package :cl-user)

(defpackage :sse-server-asd
  (:use :cl :asdf))

(in-package :sse-server-asd)

(defsystem :sse-server
  :version "0.1.0"
  :author "Dave Tenny"
  :description "sse-server implements support for the sender side of Server Side Events"
  :depends-on (:cl-ppcre :trivial-escapes)
  :components ((:file "server")))
  
