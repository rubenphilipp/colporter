;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****h* package
;;; NAME
;;; package
;;; 
;;; AUTHOR
;;; Ruben Philipp <me@rubenphilipp.com>
;;;
;;; CREATED
;;; 2023-07-09
;;; 
;;; PURPOSE
;;; Package definition for colporter.
;;;
;;;
;;; $$ Last modified:  23:03:42 Sun Jul  9 2023 CEST
;;; ****
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defpackage :colporter
  (:use :common-lisp)
  (:nicknames :clptr)
  (:import-from :alexandria :read-file-into-string)
  (:import-from :cl-ppcre :split)
  (:import-from
   :spinneret
   :with-html
   :with-html-string
   :html
   :do-elements
   :deftag
   :html-length
   :dynamic-tag
   :interpret-html-tree
   :escape-string
   :parse-as-markdown))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; EOF package.lisp
