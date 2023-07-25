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
;;; $$ Last modified:  12:21:49 Tue Jul 25 2023 CEST
;;; ****
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :colporter)

(defclass site (named-object)
  ;; a hash table containing all snippet objects available to the site
  ((snippets :accessor snippets :initarg :snippets :initform nil)
   ;; a hash table containing all asset objects available to the site
   (assets :accessor assets :initarg :assets :initform nil)
   ;; a hash table containing all template objects available to the site
   ;; please also note the doc for make-page
   (templates :accessor templates :initarg :templates :initform nil)
   ;; a hash table containing all page objects
   (pages :accessor pages :initarg :pages :initform nil)
   ;; a hash table containing all file objects related to the site
   (files :accessor files :initarg :files :initform nil)
   ;; an optional (short) description of the site
   (description :accessor description :initarg :description :initform "")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; this macro is a helper for the initialization process
;;; RP  Tue Jul 25 00:25:14 2023
(defmacro init-alist?-hash-table (slot object)
  `(let ((val
           (cond ((typep (,slot ,object) 'hash-table)
                  (,slot ,object))
                 ((alistp (,slot ,object))
                  (alist-hash-table (,slot ,object)))
                 ((null (,slot ,object))
                  nil)
                 (t (error "site::init-alist?-hash-table: The given value ~a ~
                            is neither of type HASH-TABLE nor of type ALIST, ~
                            but ~a."
                           (,slot ,object)
                           (type-of (,slot ,object)))))))
     (setf (slot-value ,object (quote ,slot)) val)))
          

(defmethod initialize-instance :after ((st site) &rest initargs)
  (declare (ignore initargs))
  ;; initialize slots
  (init-alist?-hash-table snippets st)
  (init-alist?-hash-table assets st)
  (init-alist?-hash-table templates st)
  (init-alist?-hash-table pages st)
  (init-alist?-hash-table files st)
  (when (data st)
    (init-alist?-hash-table data st)))


(defmethod print-object :before ((st site) stream)
  (format stream "~%SITE: description: ~a ~% ~
                  NB: other slots (snippets, templates etc.) are left out ~
                  here for brevity's sake."
          (description st)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; a helper macro for make-site which creates a hash-table from lists
;;; of objects
;;; It creates a hash table by using the ids of the source object as keys.
(defmacro site-list?-to-hash (obj)
  `(if (and (listp ,obj)
            (not (or (hash-table-p ,obj)
                     (alistp ,obj))))
       (loop for item in ,obj
             for id = (id item)
             with result = (make-hash-table)
             do
                (setf (gethash id result) item)
             finally (return result))
       ,obj))
            
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****f* site/make-site
;;; AUTHOR
;;; Ruben Philipp <me@rubenphilipp.com>
;;;
;;; CREATED
;;; 2023-07-24
;;; 
;;; DESCRIPTION
;;; This function is a shortcut to instantiate a site object. 
;;;
;;; ARGUMENTS
;;; - The available snippet objects. Could be given either as a list of
;;;   snippet objects, an alist or a hash table (the latter being the definitive
;;;   structure in which the values will be stored).
;;;   - If the argument is of type list, the keyword for the hash-table will be
;;;     the id of the respective snippet object.
;;;     NB: Make sure that, in this case, all object's IDs are unique.
;;;   - If the argument is of type alist or hash-table, the key will be set
;;;     according to the structure of the alist/hash-table.
;;; - The available asset objects. Data type is analogous to the snippets
;;;   argument.
;;; - The available template objects. Data type is analogous to the snippets
;;;   argument.
;;; - The available page objects. Data type is analogous to the snippets
;;;   argument.
;;; - The available file objects. Data type is analogous to the snippets
;;;   argument.
;;; 
;;; OPTIONAL ARGUMENTS
;;; keyword-arguments:
;;; - :data. The data of the site (e.g. title, author etc.) as an alist or
;;;   hash-table. Default = nil.
;;; - :description. A short textual description of the site. Default = ""
;;; - :id. The id of the site object. 
;;; 
;;; RETURN VALUE
;;; The site object. 
;;;
;;; EXAMPLE
#|
(let* ((snippets (list
                    (make-snippet #'(lambda (x y) (+ x y))
                                             :id 'sn1
                                             :description "addition")
                    (make-snippet #'(lambda (x) (print x))
                                             :id 'sn2
                                             :description "just print")))
         (assets (list
                  (make-asset
                   "/assets/style.css"
                   "style.css")))
         (templates (list
                     (cons 'home
                           (make-template
                            (define-template
                              "home")))
                     (cons 'project
                           (make-template
                            (define-template
                              "project")))))
         (base "/content/")
         (pages (list
                 (make-page
                  "/content/home.yaml"
                  base)
                 (make-page
                  "/content/projects/opus-1.yaml"
                  base)))
         (files (list
                 (make-file
                  "/content/projects/testb.jpg")
                 (make-file
                  "/content/test.jpg")))
       (site (make-site snippets assets templates
                        pages files :data '((title . "Test")))))
  site)
|#
;;; SYNOPSIS
(defun make-site (snippets assets templates pages files
                  &key
                    (data nil)
                    (description "")
                    (id nil))
  ;;; ****
  (let ((snippets (site-list?-to-hash snippets))
        (assets (site-list?-to-hash assets))
        (templates (site-list?-to-hash templates))
        (pages (site-list?-to-hash pages))
        (files (site-list?-to-hash files)))
    (make-instance 'site :snippets snippets
                         :assets assets
                         :templates templates
                         :pages pages
                         :files files
                         :data data
                         :description description
                         :id id)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****m* site/get-data
;;; AUTHOR
;;; Ruben Philipp <me@rubenphilipp.com>
;;;
;;; CREATED
;;; 2023-07-24
;;; 
;;; DESCRIPTION
;;; This method returns the value of a field in the hash-table in the data slot
;;; of the given site object. 
;;;
;;; ARGUMENTS
;;; - The site object.
;;; - The key to the value of the field in the data slot of the site object. 
;;; 
;;; RETURN VALUE
;;; The data stored in the field of the hash table. 
;;;
;;; SYNOPSIS
(defmethod get-data ((st site) key)
  ;;; ****
  (gethash key (data st)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****m* site/set-data
;;; AUTHOR
;;; Ruben Philipp <me@rubenphilipp.com>
;;;
;;; CREATED
;;; 2023-07-24
;;; 
;;; DESCRIPTION
;;; Sets the value of a field in the data hash-table. When the key is not,
;;; it will add a new key to the hash table. 
;;;
;;; ARGUMENTS
;;; - The site object.
;;; - The key of the value to change / add.
;;; - The new value of the field in the hash-table.
;;; 
;;; OPTIONAL ARGUMENTS
;;; keyword-arguments:
;;; - :warn. Issues a warning when the field in the hash-table is not present,
;;;   thus adding a new key to the list. Default = NIL. 
;;; 
;;; RETURN VALUE
;;; The value of the changed/added field.
;;;
;;; SYNOPSIS
(defmethod set-data ((st site) key value
                     &key (warn nil))
  ;;; ****
  (when (and warn
             (not (gethash key (data st))))
    (warn "site::set-data: The key ~a is not present in the :data of the ~
           site object and will be added." key))
  (setf (gethash key (data st)) value)
  value)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****m* site/remove-data
;;; AUTHOR
;;; Ruben Philipp <me@rubenphilipp.com>
;;;
;;; CREATED
;;; 2023-07-24
;;; 
;;; DESCRIPTION
;;; Removes a data field from the hash-table in the data slot of a site
;;; object. 
;;;
;;; ARGUMENTS
;;; - A site object
;;; - The key of the field to be removed. 
;;; 
;;; RETURN VALUE
;;; T when removal was successful, otherwise NIL.
;;;
;;; SYNOPSIS
(defmethod remove-data ((obj site) key)
  ;;; ****
  (remhash key (data obj)))



   

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; EOF site.lisp
