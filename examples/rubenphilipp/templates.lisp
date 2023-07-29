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
;;; $$ Last modified:  16:31:29 Sat Jul 29 2023 CEST
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :rubenphilipp)

(defvar +rp-templates+ (make-hash-table :test #'equal))

;; load all lisp files in the current dir
(load-files-by-extension (clptr:path-from-same-dir "templates/") ".lisp")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; EOF templates.lisp
