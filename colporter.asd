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
;;; $$ Last modified:  21:34:58 Sat Jul 20 2024 CEST
;;; ****
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;; main system
(defsystem "colporter"
  :description "Common Lisp static-site generator."
  :version "0.0.1"
  :author "Ruben Philipp <me@rubenphilipp.com>"
  :license "GPL Version 2.0 or later"
  :serial t
  :in-order-to ((test-op (test-op "colporter/tests")))
  :depends-on ("alexandria"
               "spinneret"
               "spinneret/cl-markdown"
               "cl-markdown"
               "cl-ppcre"
               "file-types"
               "osicat"
               "cl-fad"
               "parenscript"
               "cl-yaml"
               "local-time"
               "frugal-uuid/non-frugal")
  :pathname "src/"
  :components ((:file "package")
               (:file "named-object")
               (:file "utilities")
               (:file "markdown")
               (:file "globals")
               (:file "file")
               (:file "asset")
               (:file "snippet")
               (:file "site")
               (:file "page")
               (:file "template")
               (:file "colporter")
               ;; this needs to be loaded lastly
               ;;(:file "export-symbols")
               ))


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
