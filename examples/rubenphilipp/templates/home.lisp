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
;;; $$ Last modified:  16:22:33 Sat Jul 29 2023 CEST
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :colporter)

(setf
 (gethash "home" rp-web::+rp-templates+)
 (make-template
  (define-template
    (with-html-string
      (:doctype)
      (:html
       (insert-snippet "header"
                       (concatenate
                        'string
                        "HOME: "
                        (get-data
                         page "title")))
       (:body
        (:img :src (insert-asset-path "test.jpg")
              :style "width: 300px; height: auto;")
        (:h2 "author")
        (:p (get-data page "author"))
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
  :id "home"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; EOF home.lisp
