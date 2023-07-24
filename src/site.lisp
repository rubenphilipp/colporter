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
;;; NB: Information like the site title resides in the data slot (which
;;;     holds a hash table). 
;;;
;;; CLASS HIERARCHY
;;; named-object -> site
;;;
;;; $$ Last modified:  00:43:54 Tue Jul 25 2023 CEST
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
;;; this macro is a helper for the initialization process
;;; RP  Tue Jul 25 00:25:14 2023
(defmacro init-alist?-hash-table (slot object)
  `(let ((val
           (cond ((type-of (,slot ,object) 'hash-table)
                  (,slot ,object))
                 ((alistp (,slot ,object))
                  (alist-hash-table (,slot ,object)))
                 (t (error "site::init-alist?-hash-table: The given value ~
                            is neither of type HASH-TABLE nor of type ALIST, ~
                            but ~a."
                           (type-of (,slot ,object)))))))
     (setf (slot-value ,object (quote ,slot)) val)))
          

(defmethod initialize-instance :after ((st site) &rest initargs)
  (declare (ignore initargs))
  ;; initialize slots
  (init-alist?-hash-table snippets st)
  (init-alist?-hash-table templates st)
  (init-alist?-hash-table pages st)
  (init-alist?-hash-table files st)
  (when (data st)
    (init-alist?-hash-table data st)))


(defmethod print-object :before ((st site) stream)
  (format stream "~%SITE: description: ~a ~% ~
                  NB: other slots (snippets, templates etc.) are left out ~
                  here for bravity's sake."
          (description st)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



   

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; EOF site.lisp
