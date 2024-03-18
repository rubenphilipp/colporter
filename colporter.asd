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
;;; $$ Last modified:  19:43:07 Mon Mar 18 2024 CET
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
               (:file "site")
               (:file "page")
               (:file "template")
               (:file "colporter")
               ;; can't export symbols right now, as this leads to an
               ;; issue with testing
               ;; RP  Mon Mar 18 19:43:06 2024
               ;; (:file "export")
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
