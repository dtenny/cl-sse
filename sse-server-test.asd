(in-package :cl-user)

(defpackage :cl-sse-server-test
  (:use :cl :asdf))

(in-package :cl-sse-server-test)

(defsystem :sse-server-test
  :version "0.1.0"
  :license "MIT"
  :author "Dave Tenny"
  :description "sse-server tests"
  :depends-on (:fiveam :sse-server :trivial-escapes)
  :components ((:file "server-test")))
  
