;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****c* named-object/site
;;; NAME
;;; site
;;; 
;;; AUTHOR
;;; Ruben Philipp <me@rubenphilipp.com>
;;;
;;; CREATED
;;; 2023-07-24
;;; 
;;; PURPOSE
;;; Implementation of the site class and related methods and functions.
;;; A site object holds all information (i.e. meta-data, objects as pages
;;; templates etc.) that are relevant for assembling a site via colporter.
;;;
;;; CLASS HIERARCHY
;;; named-object -> site
;;;
;;; $$ Last modified:  00:01:02 Tue Jul 25 2023 CEST
;;; ****
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :colporter)

(defclass site (named-object)
  ;; a hash table containing all snippet objects available to the page
  ((snippets :accessor snippets :initarg :snippets :initform nil)
   ;; a hash table containing all template objects available to the page
   ;; please also note the doc for make-page
   (templates :accessor templates :initarg :templates :initform nil)
   ;; a hash table containing all page objects
   (pages :accessor pages :initarg :pages :initform nil)
   ;; a hash table containing all file objects related to the page
   (files :accessor files :initarg :files :initform nil)
   ;; an optional (short) description of the site
   (description :accessor description :initarg :description :initform "")))
   

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; EOF site.lisp
