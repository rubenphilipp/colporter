;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****c* named-object/page
;;; NAME
;;; page
;;; 
;;; AUTHOR
;;; Ruben Philipp <me@rubenphilipp.com>
;;;
;;; CREATED
;;; 2023-07-23
;;; 
;;; PURPOSE
;;; Implementation of the page class and related methods and functions.
;;; A page object contains both the data/content of the page as well as some
;;; meta information required for processing (e.g. a template object related to
;;; the page).
;;;
;;; The content slot of a page consists of the markdown-formatted content of a
;;; .md file in the respective content directory.
;;; The data slot holds all (meta-)data contained in the YAML-part of a page. 
;;;
;;; CLASS HIERARCHY
;;; named-object -> page
;;;
;;; $$ Last modified:  00:31:10 Mon Jul 24 2023 CEST
;;; ****
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :colporter)

(defclass page (named-object)
  ;; the (absolute) path to the page file (i.e. a .md file)
  ((path :accessor path :initarg :path :initform nil)
   ;; the destination path relative to the site directory
   (destination :accessor destination :initarg :destination :initform nil)
   ;; the content of the page (i.e. the markdown-formatted part)
   (content :accessor content :initarg :content :initform nil)
   ;; a template object related to this page
   (template :accessor template :initform nil)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; EOF page.lisp
