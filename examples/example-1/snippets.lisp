;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; FILE
;;; snippets.lisp
;;;
;;; NAME
;;; snippets
;;;
;;; DESCRIPTION
;;; Loads all snippets from the snippets dir.
;;;
;;; All (colporter) snippets will be stored in the +rp-snippets+ hash-table. 
;;;
;;; AUTHOR
;;; Ruben Philipp <me@rubenphilipp.com>
;;;
;;; CREATED
;;; 2023-07-28
;;;
;;; $$ Last modified:  23:39:22 Wed Mar 27 2024 CET
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :colporter)

(defvar +rp-snippets+ (make-hash-table :test #'equal))

;; load all lisp files in the current dir
(load-files-by-extension (path-from-same-dir "snippets/") ".lisp")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; EOF snippets.lisp
