;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****c* named-object/template
;;; NAME
;;; template
;;; 
;;; AUTHOR
;;; Ruben Philipp <me@rubenphilipp.com>
;;;
;;; CREATED
;;; 2023-07-23
;;; 
;;; PURPOSE
;;; Implementation of the template class and related methods and functions.
;;; A template consists of a function which takes a page and a site object as
;;; its arguments and returns a string which will be stored as the page data
;;; (thus, most likely, containing HTML markup).
;;;
;;; CLASS HIERARCHY
;;; named-object -> template
;;;
;;; $$ Last modified:  00:03:39 Mon Jul 24 2023 CEST
;;; ****
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :colporter)

(defclass template (named-object)
  ;; a short (optional) description of the template
  ((description :accessor description :initarg :description :initform nil)
   ;; the template function (e.g. a lambda function), for details see above
   (template-fun :accessor template-fun :initarg :template-fun :initform nil)))


(defmethod initialize-instance :after ((tp template) &rest initargs)
  (declare (ignore initargs))
  (unless (functionp (template-fun tp))
    (error "template::initialize-instance: The template-fun must be of type ~
            FUNCTION, not ~a."
           (type-of (template-fun tp))))
  (setf (slot-value tp 'data) (template-fun tp)))

(defmethod print-object :before ((tp template) stream)
  (format stream "~%TEMPLATE: description: ~a"
          (description tp)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****f* template/make-template
;;; AUTHOR
;;; Ruben Philipp <me@rubenphilipp.com>
;;;
;;; CREATED
;;; 2023-07-23
;;; 
;;; DESCRIPTION
;;; This function is a shorthand to instantiate a template object.
;;;
;;; ARGUMENTS
;;; The template function. Must be a (lambda) function. This function must take
;;; at least two arguments:
;;; - A page object.
;;; - A site object. 
;;; 
;;; OPTIONAL ARGUMENTS
;;; keyword-arguments:
;;; - :description. A short description of the template function as a string.
;;; - :id. The id of the template object. 
;;;
;;; RETURN VALUE
;;; The template object. 
;;; 
;;; SYNOPSIS
(defun make-template (template-fun &key
                                     (description "")
                                     (id nil))
  ;;; ****
  (make-instance 'template
                 :template-fun template-fun
                 :description description
                 :id id))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; EOF template.lisp
