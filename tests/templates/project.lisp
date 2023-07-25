;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; FILE
;;; project.lisp
;;;
;;; NAME
;;; project
;;;
;;; DESCRIPTION
;;; A template
;;;
;;; AUTHOR
;;; Ruben Philipp <me@rubenphilipp.com>
;;;
;;; CREATED
;;; 2023-07-24
;;;
;;; $$ Last modified:  18:20:53 Tue Jul 25 2023 CEST
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :colporter.tests)

(setf
 (gethash "project" +clptr-test-templates+)
 (colporter::make-template
  (colporter::define-template
    (colporter::with-html
      (:doctype)
      (:html
       (colporter::insert-snippet "header"
                                  (concatenate
                                   'string
                                   "PROJECT: "
                                   (colporter::get-data page "title")))
       (:body
        ((colporter::get-data page "content"))))))
  :id "project"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; EOF project.lisp
