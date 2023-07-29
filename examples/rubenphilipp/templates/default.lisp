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
;;; $$ Last modified:  16:22:17 Sat Jul 29 2023 CEST
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :colporter)

(setf
 (gethash "default" rp-web::+rp-templates+)
 (make-template
  (define-template
    (with-html-string
      (:doctype)
      (:html
       (insert-snippet "header"
                       (get-data page "title"))
       (:body
        (:raw
         (parse-as-markdown
          (get-data page "content")))))))
  :id "default"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; EOF default.lisp
