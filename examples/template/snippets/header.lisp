;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; FILE
;;; header.lisp
;;;
;;; NAME
;;; header
;;;
;;; DESCRIPTION
;;; The page header snippet. 
;;;
;;; AUTHOR
;;; Ruben Philipp <me@rubenphilipp.com>
;;;
;;; CREATED
;;; 2024-03-27
;;;
;;; $$ Last modified:  23:30:17 Wed Mar 27 2024 CET
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :colporter)

(add-snippet ("header" +site-snippets+ site page title)
  (with-html
    (:head
     (:meta :charset "utf-8")
     (:title title)
     ;; META
     (:meta :name "author" :content "Wiliam Loman")
     (:meta :name "viewport" :content "width=device-width, initial-scale=1")
     ;; CSS
     ;; main stylesheet
     (:link :rel "stylesheet"
       :href (insert-asset-path "css/main.css"))
     ;; JS
     ;; main JS file (e.g. for navigation/search)
     (:script :src (insert-asset-path "js/main.js")))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; EOF header.lisp
