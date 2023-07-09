;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****h* package
;;; NAME
;;; package
;;; 
;;; AUTHOR
;;; Ruben Philipp <me@rubenphilipp.com>
;;;
;;; CREATED
;;; 2023-07-09
;;; 
;;; PURPOSE
;;; Package definition for colporter. 
;;;
;;;
;;; $$ Last modified:  23:42:51 Sun Jul  9 2023 CEST
;;; ****
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;; main system
(defsystem "colporter"
  :description "Common Lisp static-site generator."
  :version "0.0.1"
  :author "Ruben Philipp <me@rubenphilipp.com>"
  :license "GPL Version 2.0 or later"
  :serial nil ;; could also be T; TODO: test/elaborate
  :in-order-to ((test-op (test-op "colporter/tests")))
  :depends-on ("alexandria"
               "spinneret"
               "spinneret/cl-markdown"
               "cl-ppcre"
               "cl-fad"
               "cl-yaml")
  :pathname "src/"
  :components ((:file "package")
               (:file "utilities")
               (:file "asset")
               (:file "snippet")
               (:file "template")
               (:file "page")
               (:file "colporter")))


;;; regression tests
(defsystem "colporter/tests"
  :description "Test suite for colporter."
  :author "Ruben Philipp <me@rubenphilipp.com>"
  :license "GPL Version 2.0 or later"
  :depends-on ("colporter"
               "fiveam")
  :pathname "tests/"
  :perform (test-op (o c) (symbol-call :colporter.tests :run-tests))
  :components ((:file "tests")))
               


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; EOF colporter.asd
