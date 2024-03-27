;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; FILE
;;; header.lisp
;;;
;;; NAME
;;; header
;;;
;;; DESCRIPTION
;;; A page header snippet. 
;;;
;;; AUTHOR
;;; Ruben Philipp <me@rubenphilipp.com>
;;;
;;; CREATED
;;; 2023-07-24
;;;
;;; $$ Last modified:  23:39:34 Wed Mar 27 2024 CET
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :colporter)

(add-snippet ("header" +rp-snippets+ site page title)
  (with-html
    (:head
     (:meta :charset "utf-8")
     (:title title)
     ;; META
     (:meta :name "author" :content "Ruben Philipp")
     (:meta :name "viewport" :content "width=device-width, initial-scale=1")
     ;; CSS
     ;; purecss
     (:link :rel "stylesheet"
            :href (insert-asset-path "css/pure/pure-min.css"))
     ;; main stylesheet
     (:link :rel "stylesheet"
            :href (insert-asset-path "css/main.css")))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; EOF header.lisp
