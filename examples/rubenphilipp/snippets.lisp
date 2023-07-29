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
;;; $$ Last modified:  16:31:37 Sat Jul 29 2023 CEST
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :rubenphilipp)

(defvar +rp-snippets+ (make-hash-table :test #'equal))

;; load all lisp files in the current dir
(load-files-by-extension (clptr:path-from-same-dir "snippets/") ".lisp")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; EOF snippets.lisp
