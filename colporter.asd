;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****h* system
;;; NAME
;;; system
;;; 
;;; AUTHOR
;;; Ruben Philipp <me@rubenphilipp.com>
;;;
;;; CREATED
;;; 2023-07-09
;;; 
;;; PURPOSE
;;; System definition for colporter. 
;;;
;;;
;;; $$ Last modified:  13:51:38 Sat Aug  5 2023 CEST
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
               "cl-markdown"
               "cl-ppcre"
               "file-types"
               "osicat"
               "cl-fad"
               "cl-yaml"
               "local-time")
  :pathname "src/"
  :components ((:file "package")
               (:file "named-object")
               (:file "utilities")
               (:file "markdown")
               (:file "globals")
               (:file "file")
               (:file "asset")
               (:file "snippet")
               (:file "template")
               (:file "page")
               (:file "site")
               (:file "colporter")
               (:file "export")))


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
;;; Export all symbols
;;; RP  Sun Jul 23 21:50:54 2023

;; (let ((package (find-package :colporter)))
;;   (do-all-symbols (symb package)
;;     (when (and (or (find-class symb nil)
;;                    (fboundp symb))
;;                (eql (symbol-package symb) package))
;;       (export symb package))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; EOF colporter.asd
