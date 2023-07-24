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
;;; A page object contains both the data/content of the page as well as 
;;; meta information required for processing (e.g. a template object related to
;;; the page).
;;;
;;; A page always relates to a source file (e.g. a .yaml file) or object (e.g.
;;; a database row), which is used to retrieve the content and the data. 
;;;
;;; The data of the page is a hash table retrieved from the (e.g. YAML) data
;;; from the source file/object.
;;;
;;; CLASS HIERARCHY
;;; named-object -> page
;;;
;;; $$ Last modified:  17:18:48 Mon Jul 24 2023 CEST
;;; ****
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :colporter)

(defclass page (named-object)
  ;; the (absolute) path to the page source file (i.e. a .yaml file)
  ((path :accessor path :initarg :path :initform nil)
   ;; the unique id (uid) of the page object
   ;; this is most likely related to the relative structure of the page
   ;; object in the data system and will be used to generate the path of the
   ;; output file (e.g. "content/projects/opus-1/project.yaml" =>
   ;; "projects/opus-1"). 
   (uid :accessor uid :initarg :uid :initform nil)
   ;; a base path used for uid-generation
   ;; this is most likely the root path for the site content (e.g. "content/")
   (base :accessor base :initarg :base :initform nil)
   ;; a template object related to this page
   (template :accessor template :initform nil)))

(defmethod initialize-instance :after ((pg page) &rest initargs)
  (declare (ignore initargs))
  (update pg))

(defmethod print-object :before ((pg page) stream)
  (format stream "~%PAGE: path: ~a, uid: ~a, ~
                  template: ~a"
          (path pg) (uid pg) (template pg)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****m* page/update
;;; AUTHOR
;;; Ruben Philipp <me@rubenphilipp.com>
;;;
;;; CREATED
;;; 2023-07-24
;;; 
;;; DESCRIPTION
;;; This method updates the slots of a page object. 
;;;
;;; ARGUMENTS
;;; A page object. 
;;; 
;;; RETURN VALUE
;;; The updated page object. 
;;; 
;;; SYNOPSIS
(defmethod update ((pg page))
  ;;; ****
  (unless (probe-file (pg page))
    (error "page::update: The file ~a does not exist."
           (path pg)))
  (unless (base pg)
    (warn "page::update: No :base is set for the page.")))







;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; EOF page.lisp
