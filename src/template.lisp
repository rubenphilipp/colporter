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
;;; $$ Last modified:  01:12:20 Tue Jul 25 2023 CEST
;;; ****
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :colporter)

(defclass template (named-object)
  ;; a short (optional) description of the template
  ((description :accessor description :initarg :description :initform nil)
   ;; the template function (e.g. a lambda function), for details see above
   (template :accessor template :initarg :template :initform nil)))


(defmethod initialize-instance :after ((tp template) &rest initargs)
  (declare (ignore initargs))
  (unless (functionp (template tp))
    (error "template::initialize-instance: The template must be of type ~
            FUNCTION, not ~a."
           (type-of (template tp))))
  (setf (slot-value tp 'data) (template tp)))

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
;;; The template (function). Must be a (lambda) function. This function must 
;;; take at least two arguments:
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
(defun make-template (template
                      &key
                        (description "")
                        (id nil))
  ;;; ****
  (make-instance 'template
                 :template template
                 :description description
                 :id id))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****** template/define-template
;;; AUTHOR
;;; Ruben Philipp <me@rubenphilipp.com>
;;;
;;; CREATED
;;; 2023-07-24
;;; 
;;; DESCRIPTION
;;; This macro expands to a function definition to be used to create a template
;;; function (i.e. a function which takes a page and a site as arguments).
;;; 
;;; As the aforementioned objects are available when the template function is
;;; evaluated during the context of assembling a colporter-site, all related
;;; methods and the slots of page and site objects (via the `site` and `page`
;;; symbols are available in the body of the template function. 
;;;
;;; ARGUMENTS
;;; - The body of the template (function). 
;;; 
;;; RETURN VALUE
;;; Expands to a lambda function. 
;;;
;;; EXAMPLE
#|
(define-template
  (with-html
    (:p (content page))))

;; => (LAMBDA (SITE PAGE)
;;     (EVAL
;;      (WITH-HTML
;;        (:P (CONTENT PAGE)))))
|#
;;; SYNOPSIS
(defmacro define-template (&body body)
  ;;; ****
  `(lambda (site page)
     (eval ,@body)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****m* template/do-template
;;; AUTHOR
;;; Ruben Philipp <me@rubenphilipp.com>
;;;
;;; CREATED
;;; 2023-07-24
;;; 
;;; DESCRIPTION
;;; This method evaluates the template with the given page and site object. 
;;;
;;; ARGUMENTS
;;; - The template object.
;;; - A page object.
;;; - A site object. 
;;; 
;;; RETURN VALUE
;;; The return value of the template. 
;;;
;;; SYNOPSIS
(defmethod do-template ((template template) page site)
  ;;; ****
  ;; sanity checks
  (unless (typep page 'page)
    (error "template::do-template: The page must be of type PAGE, not ~a."
           (type-of page)))
  (unless (typep site 'site)
    (error "template::do-template: The site must be of type SITE, not ~a."
           (type-of site)))
  (funcall (template tp) pg st))





;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; EOF template.lisp
