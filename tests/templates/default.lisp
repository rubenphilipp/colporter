;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; FILE
;;; default.lisp
;;;
;;; NAME
;;; default
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
;;; $$ Last modified:  18:38:10 Tue Jul 25 2023 CEST
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :colporter.tests)

(setf
 (gethash "default" +clptr-test-templates+)
 (colporter::make-template
  (colporter::define-template
    (colporter::with-html
      (:doctype)
      (:html
       (colporter::insert-snippet "header"
                                  (colporter::get-data page "title"))
       (:body
        ((colporter::get-data page "content"))))))
  :id "default"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; EOF default.lisp
