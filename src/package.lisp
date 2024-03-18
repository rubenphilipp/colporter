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
;;; $$ Last modified:  23:27:21 Sun Mar 17 2024 CET
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
   :parenscript
   :ps)
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
