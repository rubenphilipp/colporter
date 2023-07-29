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
;;; $$ Last modified:  16:25:08 Sat Jul 29 2023 CEST
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :colporter)

(setf
 (gethash "project" rp-web::+rp-templates+)
 (make-template
  (define-template
    (spinneret:with-html-string
      (:doctype)
      (:html
       (insert-snippet "header"
                       (concatenate
                        'string
                        "PROJECT: "
                        (get-data
                         page "title")))
       (:body
        (:raw
         (parse-as-markdown
          (get-data page "content")))))))
  :id "project"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; EOF project.lisp
