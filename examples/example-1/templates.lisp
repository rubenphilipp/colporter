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
;;; All (colporter) templates will be stored in the +rp-templates+ hash-table. 
;;;
;;; AUTHOR
;;; Ruben Philipp <me@rubenphilipp.com>
;;;
;;; CREATED
;;; 2023-07-28
;;;
;;; $$ Last modified:  23:38:07 Wed Mar 27 2024 CET
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :colporter)

(defvar +rp-templates+ (make-hash-table :test #'equal))

;; load all lisp files in the current dir
(load-files-by-extension (path-from-same-dir "templates/") ".lisp")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; EOF templates.lisp
