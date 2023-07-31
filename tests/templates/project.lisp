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
;;; $$ Last modified:  09:22:02 Mon Jul 31 2023 CEST
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :colporter.tests)

(setf
 (gethash "project" +clptr-test-templates+)
 (colporter::make-template
  (colporter::define-template
    (colporter::with-html-string
      (:doctype)
      (:html
       (colporter::insert-snippet "header"
                                  (concatenate
                                   'string
                                   "PROJECT: "
                                   (colporter::get-data
                                    colporter::page "title")))
       (:body
        (:raw
         (colporter::with-colportage
          (colporter::get-data colporter::page "content")))))))
  :id "project"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; EOF project.lisp
