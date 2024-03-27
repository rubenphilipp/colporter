;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; FILE
;;; default.lisp
;;;
;;; NAME
;;; default
;;;
;;; DESCRIPTION
;;; Default template.
;;;
;;; AUTHOR
;;; Ruben Philipp <me@rubenphilipp.com>
;;;
;;; CREATED
;;; 2024-03-27
;;;
;;; $$ Last modified:  23:30:28 Wed Mar 27 2024 CET
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :colporter)

(add-template ("default" +site-templates+)
  (with-html-string
    (:doctype)
    (:html
     (insert-snippet "header"
                     site page (get-data page "title"))
     (:body
      (:raw
       (parse-as-markdown
        (get-data page "content")))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; EOF default.lisp
