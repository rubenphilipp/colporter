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
;;; $$ Last modified:  23:34:06 Tue Jul 25 2023 CEST
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
        (:img :src (colporter::insert-asset-path "test.jpg")
              :style "width: 300px; height: auto;")
        (:h2 "author")
        (:p (colporter::get-data colporter::page "author"))
        (:h3 "keywords")
        (:div
         (loop for keyword in (colporter::get-data
                               colporter::page "keywords")
               with result = ""
               do
                  (setf result (concatenate 'string
                                            result
                                            " "
                                            keyword))
               finally (return result)))
        (:raw
         (colporter::with-colportage
             (colporter::get-data colporter::page "content")))))))
  :id "home"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; EOF home.lisp
