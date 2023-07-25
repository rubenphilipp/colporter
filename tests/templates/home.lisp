;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; FILE
;;; home.lisp
;;;
;;; NAME
;;; home
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
;;; $$ Last modified:  19:36:27 Tue Jul 25 2023 CEST
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :colporter.tests)

(setf
 (gethash "home" +clptr-test-templates+)
 (colporter::make-template
  (colporter::define-template
    (colporter::with-html-string
      (:doctype)
      (:html
       (colporter::insert-snippet "header"
                                  (concatenate
                                   'string
                                   "HOME: "
                                   (colporter::get-data
                                    colporter::page "title")))
       (:body
        (:raw
         (colporter::parse-as-markdown
          (colporter::get-data colporter::page "content")))))))
  :id "home"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; EOF home.lisp
