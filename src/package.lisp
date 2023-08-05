;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****h* clptr/package
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
;;; $$ Last modified:  14:10:27 Sat Aug  5 2023 CEST
;;; ****
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :cl-user)

(defpackage :colporter
  (:use :common-lisp)
  (:nicknames :clptr)
  (:import-from
   :alexandria
   :assoc-value
   :alist-hash-table
   :hash-table-alist
   :hash-table-keys
   :read-file-into-string)
  (:import-from
   :cl-ppcre
   :split)
  (:import-from
   :local-time
   ;;:parse-timestring
   :timestamp>
   :timestamp>=
   :timestamp<
   :timestamp<=
   :timestamp/=
   :timestamp=)
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
