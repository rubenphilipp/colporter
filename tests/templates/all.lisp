;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; FILE
;;; all.lisp
;;;
;;; NAME
;;; load all test templates
;;;
;;; DESCRIPTION
;;; This file loads all test templates into the +clptr-test-templates+
;;; hash-table.
;;;
;;; AUTHOR
;;; Ruben Philipp <me@rubenphilipp.com>
;;;
;;; CREATED
;;; 2023-07-24
;;;
;;; $$ Last modified:  18:36:06 Tue Jul 25 2023 CEST
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :colporter.tests)

(defvar +clptr-test-templates+ (make-hash-table :test #'equal))

(defmacro load-template (file)
  `(load (concatenate 'string
                      basedir
                      ,file)))

(let ((basedir (test-pathname "templates/")))
  (load-template "default.lisp")
  (load-template "home.lisp")
  (load-template "project.lisp"))
 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; EOF all.lisp
