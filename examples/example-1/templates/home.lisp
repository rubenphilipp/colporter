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
;;; $$ Last modified:  23:38:50 Wed Mar 27 2024 CET
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :colporter)

(add-template ("home" +rp-template+)
  (with-html-string
    (:doctype)
    (:html
     (insert-snippet "header"
                     site page (concatenate
                                'string
                                "HOME: "
                                (get-data
                                 page "title")))
     (:body
      (:img :src (insert-asset-path "test.jpg")
            :style "width: 300px; height: auto;")
      (:h2 "author")
      (:p (get-data page "author"))
      ;; (:p (insert-asset-path "css/pure/pure-min.css"))
      (:h3 "keywords")
      (:div
       (loop for keyword in (get-data
                             page "keywords")
             with result = ""
             do
                (setf result (concatenate 'string
                                          result
                                          " "
                                          keyword))
             finally (return result)))
      (:raw
       (with-colportage
           (get-data page "content")))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; EOF home.lisp
