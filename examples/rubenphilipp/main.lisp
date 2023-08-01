;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; FILE
;;; main.lisp
;;;
;;; NAME
;;; main
;;;
;;; DESCRIPTION
;;; The main file for generating Ruben Philipp's website. 
;;;
;;; AUTHOR
;;; Ruben Philipp <me@rubenphilipp.com>
;;;
;;; CREATED
;;; 2023-07-28
;;;
;;; $$ Last modified:  23:23:52 Tue Aug  1 2023 CEST
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(ql:quickload :colporter)

(in-package :colporter)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; load relevant files

;; package definition
(load (path-from-same-dir "package.lisp"))

;; load utilities
(load (path-from-same-dir "utilities.lisp"))
;; load templates
(load (path-from-same-dir "templates.lisp"))
;; load snippets
(load (path-from-same-dir "snippets.lisp"))
;; now load the main construction file
(load (path-from-same-dir "rubenphilipp.lisp"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; EOF main.lisp
