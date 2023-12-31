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
;;; $$ Last modified:  17:31:39 Wed Oct 18 2023 CEST
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
   ;; the base directory for the assets (relative to output-dir; cf. colporter)
   (asset-base-dir :accessor asset-base-dir :initarg :asset-base-dir
                   :initform "")
   (templates :accessor templates :initarg :templates :initform nil)
   ;; a hash table containing all page objects
   (pages :accessor pages :initarg :pages :initform nil)
   ;; a hash table containing all file objects related to the site
   (files :accessor files :initarg :files :initform nil)
   ;; the page root (will be added to all paths generated during the rendering
   ;; processes)
   ;; RP  Wed Oct 18 17:24:08 2023
   (page-root :accessor page-root :initarg :page-root :initform "/")
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
                  (alist-hash-table (,slot ,object) :test 'equal))
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
    (init-alist?-hash-table data st))
  (update st))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; update the object

(defmethod update ((st site))
  ;; trailing slash for asset-base-dir
  (setf (slot-value st 'asset-base-dir) (trailing-slash (asset-base-dir st)))
  ;; trailing slash for page-root
  (setf (slot-value st 'page-root) (trailing-slash (page-root st)))
  st)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod (setf asset-base-dir) (value (st site))
  (declare (ignore value))
  (update st))

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
             with result = (make-hash-table :test 'equal)
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
;;; - :asset-base-dir. A string being the base directory for the assets
;;;   (relative to the output-dir; cf. colporter). Default = "assets/"
;;; - :data. The data of the site (e.g. title, author etc.) as an alist or
;;;   hash-table. Default = nil.
;;; - :description. A short textual description of the site. Default = ""
;;; - :id. The id of the site object.
;;; - :page-root. The root of the page. Will be added to any path generated
;;;   via the rendering processes. Must be a string. Default = "/"
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
                    (asset-base-dir "assets/")
                    (data nil)
                    (description "")
                    (id nil)
                    (page-root "/"))
  ;;; ****
  (let ((snippets (site-list?-to-hash snippets))
        (assets (site-list?-to-hash assets))
        (templates (site-list?-to-hash templates))
        (pages (site-list?-to-hash pages))
        (files (site-list?-to-hash files)))
    (make-instance 'site :snippets snippets
                         :assets assets
                         :asset-base-dir asset-base-dir
                         :templates templates
                         :pages pages
                         :files files
                         :data data
                         :page-root page-root
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
(defmethod get-data ((obj site) key)
  ;;; ****
  (gethash key (data obj)))


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
;;; ****m* site/get-snippet
;;; AUTHOR
;;; Ruben Philipp <me@rubenphilipp.com>
;;;
;;; CREATED
;;; 2023-07-24
;;; 
;;; DESCRIPTION
;;; Returns a snippet object from the snippet hash-table of a site object by
;;; its key. 
;;;
;;; ARGUMENTS
;;; - The site object.
;;; - The key of the snippet according to the hash-table. 
;;; 
;;; RETURN VALUE
;;; The snippet-object. 
;;;
;;; SYNOPSIS
(defmethod get-snippet ((obj site) key)
  ;;; ****
  (gethash key (snippets obj)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****m* site/get-asset
;;; AUTHOR
;;; Ruben Philipp <me@rubenphilipp.com>
;;;
;;; CREATED
;;; 2023-07-24
;;; 
;;; DESCRIPTION
;;; Returns a asset object from the asset hash-table of a site object by
;;; its key. 
;;;
;;; ARGUMENTS
;;; - The site object.
;;; - The key of the asset according to the hash-table. 
;;; 
;;; RETURN VALUE
;;; The asset-object. 
;;;
;;; SYNOPSIS
(defmethod get-asset ((obj site) key)
  ;;; ****
  (let ((asset (gethash key (assets obj))))
    (unless asset
      (error "site::get-asset: The asset with the key \"~a\" does not exist."
             key))
    asset))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****m* site/get-file
;;; AUTHOR
;;; Ruben Philipp <me@rubenphilipp.com>
;;;
;;; CREATED
;;; 2023-07-24
;;; 
;;; DESCRIPTION
;;; Returns a file object from the file hash-table of a site object by
;;; its key. 
;;;
;;; ARGUMENTS
;;; - The site object.
;;; - The key of the file according to the hash-table. 
;;; 
;;; RETURN VALUE
;;; The file-object. 
;;;
;;; SYNOPSIS
(defmethod get-file ((obj site) key)
  ;;; ****
  (let ((file (gethash key (files obj))))
    (unless file
      (error "site::get-file: The file with the key \"~a\" does not exist."
             key))
    file))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****m* site/get-template
;;; AUTHOR
;;; Ruben Philipp <me@rubenphilipp.com>
;;;
;;; CREATED
;;; 2023-07-24
;;; 
;;; DESCRIPTION
;;; Returns a template object from the template hash-table of a site object by
;;; its key. 
;;;
;;; ARGUMENTS
;;; - The site object.
;;; - The key of the template according to the hash-table. 
;;; 
;;; RETURN VALUE
;;; The template-object. 
;;;
;;; SYNOPSIS
(defmethod get-template ((obj site) key)
  ;;; ****
  (gethash key (templates obj)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****m* site/get-page
;;; AUTHOR
;;; Ruben Philipp <me@rubenphilipp.com>
;;;
;;; CREATED
;;; 2023-07-24
;;; 
;;; DESCRIPTION
;;; Returns a page object from the page hash-table of a site object by
;;; its key. 
;;;
;;; ARGUMENTS
;;; - The site object.
;;; - The key of the page according to the hash-table. 
;;; 
;;; RETURN VALUE
;;; The page-object. 
;;;
;;; SYNOPSIS
(defmethod get-page ((obj site) key)
  ;;; ****
  (gethash key (pages obj)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****m* site/get-page-by-uuid
;;; AUTHOR
;;; Ruben Philipp <me@rubenphilipp.com>
;;;
;;; CREATED
;;; 2023-07-30
;;; 
;;; DESCRIPTION
;;; This method returns a page object from the page hash-table of a site object
;;; by its uuid data slot.
;;;
;;; ARGUMENTS
;;; - The site object.
;;; - The uuid (cf. make-page) of the page.
;;; 
;;; RETURN VALUE
;;; The page object. 
;;; 
;;; SYNOPSIS
(defmethod get-page-by-uuid ((obj site) uuid)
  ;;; ****
  (loop for page being the hash-values of (pages obj)
        when (equal uuid (uuid page))
          do
             (return page)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****m* site/get-keys-or-objects
;;; AUTHOR
;;; Ruben Philipp <me@rubenphilipp.com>
;;;
;;; CREATED
;;; 2023-07-24
;;; 
;;; DESCRIPTION
;;; A helper macro for most of the getter shorthand methods (e.g.
;;; get-snippets) which returns either a list of the keys in the hash-table
;;; or the objects themselves (also cf. get-snippets). 
;;;
;;; ARGUMENTS
;;; - A site object.
;;; - The accessor of the slot containing the hash-table.
;;; - A boolean which indicates whether a list of keys (NIL) or objects (T)
;;;   should be returned.
;;;
;;; EXAMPLE
#|
(get-keys-or-objects site assets nil)
;; => 
;; (LET ((DATA (GETHASH (ASSETS SITE))))
;;   (IF NIL
;;       (LOOP FOR VALUE BEING THE HASH-VALUES OF DATA
;;             WITH RESULT = 'NIL
;;             DO (PUSH VALUE RESULT)
;;             FINALLY (RETURN (REVERSE RESULT)))
;;       (HASH-TABLE-KEYS DATA)))
|#
;;; SYNOPSIS
(defmacro get-keys-or-objects (obj slot objects?)
  ;;; ****
  `(let ((data (,slot ,obj)))
     (if ,objects?
         (loop for value being the hash-values of data
               with result = '()
               do (push value result)
               finally (return (reverse result)))
         (hash-table-keys data))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****m* site/get-snippets
;;; AUTHOR
;;; Ruben Philipp <me@rubenphilipp.com>
;;;
;;; CREATED
;;; 2023-07-24
;;; 
;;; DESCRIPTION
;;; Returns either a list of all keys related to the snippets objects in the
;;; snippets hash-table or, when :objects is T, a list of the objects
;;; themselves. 
;;;
;;; ARGUMENTS
;;; - A site object.
;;; 
;;; OPTIONAL ARGUMENTS
;;; keyword-arguments:
;;; - :objects. A boolean indicating whether returning a list of keys (NIL) or
;;;   a list of objects (T). Default = NIL
;;; 
;;; RETURN VALUE
;;; Either a list of keys or snippet objects. 
;;;
;;; SYNOPSIS
(defmethod get-snippets ((obj site) &key (objects nil))
  ;;; ****
  (get-keys-or-objects obj snippets objects))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****m* site/get-assets
;;; AUTHOR
;;; Ruben Philipp <me@rubenphilipp.com>
;;;
;;; CREATED
;;; 2023-07-24
;;; 
;;; DESCRIPTION
;;; Returns either a list of all keys related to the assets objects in the
;;; assets hash-table or, when :objects is T, a list of the objects
;;; themselves. 
;;;
;;; ARGUMENTS
;;; - A site object.
;;; 
;;; OPTIONAL ARGUMENTS
;;; keyword-arguments:
;;; - :objects. A boolean indicating whether returning a list of keys (NIL) or
;;;   a list of objects (T). Default = NIL
;;; 
;;; RETURN VALUE
;;; Either a list of keys or snippet objects. 
;;;
;;; SYNOPSIS
(defmethod get-assets ((obj site) &key (objects nil))
  ;;; ****
  (get-keys-or-objects obj assets objects))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****m* site/get-templates
;;; AUTHOR
;;; Ruben Philipp <me@rubenphilipp.com>
;;;
;;; CREATED
;;; 2023-07-24
;;; 
;;; DESCRIPTION
;;; Returns either a list of all keys related to the templates objects in the
;;; templates hash-table or, when :objects is T, a list of the objects
;;; themselves. 
;;;
;;; ARGUMENTS
;;; - A site object.
;;; 
;;; OPTIONAL ARGUMENTS
;;; keyword-arguments:
;;; - :objects. A boolean indicating whether returning a list of keys (NIL) or
;;;   a list of objects (T). Default = NIL
;;; 
;;; RETURN VALUE
;;; Either a list of keys or snippet objects. 
;;;
;;; SYNOPSIS
(defmethod get-templates ((obj site) &key (objects nil))
  ;;; ****
  (get-keys-or-objects obj templates objects))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****m* site/get-pages
;;; AUTHOR
;;; Ruben Philipp <me@rubenphilipp.com>
;;;
;;; CREATED
;;; 2023-07-24
;;; 
;;; DESCRIPTION
;;; Returns either a list of all keys related to the pages objects in the
;;; pages hash-table or, when :objects is T, a list of the objects
;;; themselves. 
;;;
;;; ARGUMENTS
;;; - A site object.
;;; 
;;; OPTIONAL ARGUMENTS
;;; keyword-arguments:
;;; - :objects. A boolean indicating whether returning a list of keys (NIL) or
;;;   a list of objects (T). Default = NIL
;;; 
;;; RETURN VALUE
;;; Either a list of keys or snippet objects. 
;;;
;;; SYNOPSIS
(defmethod get-pages ((obj site) &key (objects nil))
  ;;; ****
  (get-keys-or-objects obj pages objects))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****m* site/get-files
;;; AUTHOR
;;; Ruben Philipp <me@rubenphilipp.com>
;;;
;;; CREATED
;;; 2023-07-24
;;; 
;;; DESCRIPTION
;;; Returns either a list of all keys related to the files objects in the
;;; files hash-table or, when :objects is T, a list of the objects
;;; themselves. 
;;;
;;; ARGUMENTS
;;; - A site object.
;;; 
;;; OPTIONAL ARGUMENTS
;;; keyword-arguments:
;;; - :objects. A boolean indicating whether returning a list of keys (NIL) or
;;;   a list of objects (T). Default = NIL
;;; 
;;; RETURN VALUE
;;; Either a list of keys or snippet objects. 
;;;
;;; SYNOPSIS
(defmethod get-files ((obj site) &key (objects nil))
  ;;; ****
  (get-keys-or-objects obj files objects))
   

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; EOF site.lisp
