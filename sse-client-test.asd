(in-package :cl-user)

(defpackage :cl-sse-client-test
  (:use :cl :asdf))

(in-package :cl-sse-client-test)

(defsystem :sse-client-test
  :version "0.1.0"
  :license "MIT"
  :author "Dave Tenny"
  :description "sse-client tests"
  :depends-on (:fiveam :sse-client :trivial-escapes)
  :components ((:file "client-test")))
  
