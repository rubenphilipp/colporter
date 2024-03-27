;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; FILE
;;; templates.lisp
;;;
;;; NAME
;;; templates
;;;
;;; DESCRIPTION
;;; Loads all templates from the templates dir.
;;;
;;; All (colporter) templates will be stored in the +site-templates+ hash-table.
;;;
;;; AUTHOR
;;; Ruben Philipp <me@rubenphilipp.com>
;;;
;;; CREATED
;;; 2024-03-27
;;;
;;; $$ Last modified:  23:29:42 Wed Mar 27 2024 CET
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :colporter)

(defvar +site-templates+ (make-hash-table :test #'equal))

;; load all lisp files in the current dir
(load-files-by-extension (path-from-same-dir "templates/") ".lisp")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; EOF templates.lisp
