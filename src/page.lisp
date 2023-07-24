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
;;; meta information required for processing (e.g. a template id related to
;;; a template object which will be used to render the page).
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
;;; $$ Last modified:  18:34:01 Mon Jul 24 2023 CEST
;;; ****
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :colporter)

(defclass page (named-object)
  ;; the (absolute) path to the page source file (i.e. a .yaml file)
  ((path :accessor path :initarg :path :initform nil)
   ;; the unique id (uid) of the page object
   ;; this is most likely related to the relative structure of the page
   ;; object in the data system and will be used to generate the path of the
   ;; output file (e.g. "content/projects/opus-1.yaml" =>
   ;; "projects/opus-1"). 
   (uid :accessor uid :initarg :uid :initform nil)
   ;; a base path used for uid-generation
   ;; this is most likely the root path for the site content (e.g. "content/")
   (base :accessor base :initarg :base :initform nil)
   ;; a template id related to this page
   (template :accessor template :initarg :template  :initform nil)))

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
  (unless (probe-file (path pg))
    (error "page::update: The file ~a does not exist."
           (path pg)))
  (unless (base pg)
    (warn "page::update: No :base is set for the page. Thus, the UID might ~
           be meaningless."))
  ;; get data
  (let* ((data-string (read-file-into-string (path pg)))
         (yaml-data (cl-yaml::parse data-string)))
    (setf (slot-value pg 'data) yaml-data))
  ;; get template from data
  (unless (template pg)
    (let ((template-id (gethash (get-clptr-config :template-key)
                                (data pg))))
      (when template-id
        (setf (slot-value pg 'template)
              (if (stringp template-id)
                  (read-from-string template-id)
                  template-id)))))
  ;; set uid
  (unless (uid pg)
    (setf (slot-value pg 'uid) (uid-from-path (path pg) (base pg))))
  pg)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****f* page/make-page
;;; AUTHOR
;;; Ruben Philipp <me@rubenphilipp.com>
;;;
;;; CREATED
;;; 2023-07-24
;;; 
;;; DESCRIPTION
;;; This function is a shortcut to instantiate a page object. 
;;;
;;; ARGUMENTS
;;; - The (absolute) path to the file used to retrieve the page data.
;;;   Should be (as of 2023-07-24) a YAML-file. Path is given as a string.
;;; - The base path to the content directory. This is the directory that
;;;   holds the data structure which will also reflect the data structure of
;;;   the site generated via colporter. 
;;; 
;;; OPTIONAL ARGUMENTS
;;; keyword-arguments:
;;; - :uid. A UID of the page. It is recommended to leave this blank, as the
;;;   UID will be generated using the path and the base path.
;;;   E.g.: "/sites/rubenphilipp/content/projects/opus-1.yaml"
;;;   with base="/sites/rubenphilipp/content/" => "projects/opus-1"
;;;   (cf. uid-from-path).
;;; - :template. A template id referring to the template object which will be
;;;   used to render the page. If NIL, the template will be automatically
;;;   selected based on the data of the page or the default template.
;;;   Must be a symbol. Default = NIL.
;;; 
;;; RETURN VALUE
;;; The page object. 
;;;
;;; SYNOPSIS
(defun make-page (path base-path &key
                                   (uid nil)
                                   (template nil))
  ;;; ****
  (make-instance 'page :path path
                       :base base-path
                       :uid uid
                       :template template))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; EOF page.lisp
