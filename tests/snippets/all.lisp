;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; FILE
;;; all.lisp
;;;
;;; NAME
;;; test snippets
;;;
;;; DESCRIPTION
;;; This file loads all test snippets into the +clptr-test-snippets+
;;; hash-table.
;;;
;;; AUTHOR
;;; Ruben Philipp <me@rubenphilipp.com>
;;;
;;; CREATED
;;; 2023-07-24
;;;
;;; $$ Last modified:  18:36:48 Tue Jul 25 2023 CEST
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :colporter.tests)

(defvar +clptr-test-snippets+ (make-hash-table :test #'equal))

(defmacro load-asset (file)
  `(load (concatenate 'string
                      basedir
                      ,file)))

(let ((basedir (test-pathname "snippets/")))
  (load-asset "header.lisp"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; EOF all.lisp
