;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; FILE
;;; header.lisp
;;;
;;; NAME
;;; header
;;;
;;; DESCRIPTION
;;; A page header snippet. 
;;;
;;; AUTHOR
;;; Ruben Philipp <me@rubenphilipp.com>
;;;
;;; CREATED
;;; 2023-07-24
;;;
;;; $$ Last modified:  18:15:49 Tue Jul 25 2023 CEST
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :colporter.tests)

(setf
 (gethash "header" +clptr-test-snippets+)
 (colporter::make-snippet
  (colporter::define-snippet (title)
    (:head
     (:title title)))
  :id "header"))
  

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; EOF header.lisp
