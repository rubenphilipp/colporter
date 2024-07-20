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
;;; $$ Last modified:  18:30:29 Sat Jul 20 2024 CEST
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
   ;; a Universally Unique IDentifier for the page (optional) which could be
   ;; used for creating permalinks or referencing to a page independent of the
   ;; uid / hierarchical page structure
   ;; a uuid should be explicitly assigned to a page object and be -- for the
   ;; sake of consistency -- persisted e.g. in the page-consituent file (e.g.
   ;; a YAML file)
   (uuid :accessor uuid :initarg :uuid :initform nil)
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
         (yaml-data (handler-case (cl-yaml::parse data-string)
                      (error (c)
                        (format t "page::update: YAML syntax is invalid. Check ~
                                your YAML data file.")
                        c))))
    (setf (slot-value pg 'data) yaml-data))
  ;; get template from data
  (unless (template pg)
    (let ((template-id (gethash (get-clptr-config :template-key)
                                (data pg))))
      (when template-id
        (setf (slot-value pg 'template) template-id))))
  ;; set uid
  (unless (uid pg)
    (setf (slot-value pg 'uid) (uid-from-path (path pg) (base pg))))
  ;; set id according to uid
  (setf (slot-value pg 'id) (uid pg))
  ;; set uuid when the YAML file contains a "uuid" element
  ;; RP  Sun Jul 30 00:43:20 2023
  (when (gethash "uuid" (data pg))
    (setf (slot-value pg 'uuid) (gethash "uuid" (data pg))))
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
;;; - :uuid. The Unique IDentifier for the page. This value could be used for
;;;   creating permalinks or referencing to a page independent of the uid /
;;;   hierarchical page structure of a site. A uuid should be explicitly
;;;   assigned to a page object and be -- for the sake of consistency --
;;;   persisted, e.g. in the page-constituing file (i.e. most likely a YAML
;;;   file).
;;;   NB: colporter will automatically look for a value in the page data
;;;   hash-table field "uuid". When given, it will be automatically assigned,
;;;   unless a different value has been assigned, e.g. via this method. 
;;;   NB 2: In case when working on page's contents with Emacs, uuidgen might
;;;   be helpful when creating uuids to be stored, e.g. in a "uuid"-field in
;;;   the YAML file of a page. (cf. https://github.com/kanru/uuidgen-el)
;;;   Default = nil.
;;; - :template. A template id referring to the template object which will be
;;;   used to render the page. If NIL, the template will be automatically
;;;   selected based on the data of the page or the default template.
;;;   NB: In order to build the page with the processes suggested in
;;;   colporter.lisp, the value of this slot should correspond to a key
;;;   in the :pages slot in a colporter object. 
;;;   Must be a symbol. Default = NIL.
;;; 
;;; RETURN VALUE
;;; The page object. 
;;;
;;; SYNOPSIS
(defun make-page (path base-path &key
                                   (uid nil)
                                   (uuid nil)
                                   (template nil))
  ;;; ****
  (make-instance 'page :path path
                       :base base-path
                       :uuid uuid
                       :uid uid
                       :template template))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****m* page/get-data
;;; AUTHOR
;;; Ruben Philipp <me@rubenphilipp.com>
;;;
;;; CREATED
;;; 2023-07-24
;;; 
;;; DESCRIPTION
;;; Returns the data of a given key from the YAML hash table contained in the
;;; data slot of a page object. 
;;;
;;; ARGUMENTS
;;; - The page object.
;;; - The key to the data field.
;;; 
;;; RETURN VALUE
;;; The data contained in the slot of the data hash-table. 
;;; 
;;; SYNOPSIS
(defmethod get-data ((obj page) key)
  ;;; ****
  (gethash key (data obj)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****f* page/make-pages-from-dir
;;; AUTHOR
;;; Ruben Philipp <me@rubenphilipp.com>
;;;
;;; CREATED
;;; 2023-07-24
;;; 
;;; DESCRIPTION
;;; This function recursively scans a given directory for all files that match
;;; the common page suffix (e.g. "yaml") and instantiates page objects with
;;; the respective uid set according to the given (base) path. Finally, the
;;; generated objects will be returned as a hash-table with keys set to the
;;; uids to be further used in a site object. 
;;; 
;;; ARGUMENTS
;;; A string being the path to the directory to recursively scan for page files.
;;; 
;;; OPTIONAL ARGUMENTS
;;; keyword-arguments:
;;; - :page-suffix. A string being the file suffix of pages which will be
;;;   included from the search. Default = The :page-suffix from
;;;   +clptr-config-data+. 
;;; 
;;; RETURN VALUE
;;; A hash-table with page objects (see above and cf. site).
;;;
;;; EXAMPLE
#|
(let ((pgs (make-pages-from-dir         ; ; ; ;
"/Users/rubenphilipp/lisp/colporter/tests/content" ; ; ; ;
:page-suffix "yaml")))                  ; ; ; ;
(hash-table-keys pgs))                  ; ; ; ;
;; => ("error" "home" "projects/bla/dings" "projects/opus-1") ; ; ; ;
|#
;;; SYNOPSIS
(defun make-pages-from-dir (dir
                            &key
                              (page-suffix (get-clptr-config :page-suffix)))
;;; ****
  (let ((dir (trailing-slash dir))
        (page-file-paths nil)
        (pages (make-hash-table :test 'equal)))
    ;; scan page files
    (cl-fad:walk-directory dir
                           #'(lambda (name)
                               (when (and
                                      (equal (pathname-type name)
                                             page-suffix)
                                      ;; do not include hidden files
                                      (not (equal "."
                                                  (subseq
                                                   (file-namestring name)
                                                   0 1))))
                                 (push name page-file-paths)))
                           :directories nil)
    ;; generate page objects
    (loop for path in page-file-paths
          for uid = (uid-from-path path dir)
          for page = (make-page path dir :uid uid)
          do
             (setf (gethash uid pages) page))
    pages))





;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; EOF page.lisp
