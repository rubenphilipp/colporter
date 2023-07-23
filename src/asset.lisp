;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****c* named-object/asset
;;; NAME
;;; asset
;;; 
;;; AUTHOR
;;; Ruben Philipp <me@rubenphilipp.com>
;;;
;;; CREATED
;;; 2023-07-23
;;; 
;;; PURPOSE
;;; Implementation of the asset class and related methods. An asset is a static
;;; file which is available globally on all pages (...) of the site.
;;; 
;;; NB: The contents of the files (esp. as they might be binaries) will not
;;; be stored in asset objects. 
;;;
;;; CLASS HIERARCHY
;;; named-object -> asset
;;;
;;; $$ Last modified:  22:47:24 Sun Jul 23 2023 CEST
;;; ****
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :colporter)

(defclass asset (named-object)
  ;; the (absolute) path to the asset file
  ((path :accessor path :initarg :path :initform nil)
   ;; a simple (optional) textual description of the asset
   (description :accessor description :initarg :description :initform "")
   ;; the file type (to be retrieved via file-types::file-mime)
   (type :accessor type :initform nil)
   ;; the output path relative to the site asset directory (e.g. "css/main.css")
   (destination :accessor destination :initarg :destination :initform nil)))

(defmethod initialize-instance :after ((as asset) &rest initargs)
  (declare (ignore initargs))
  (setf (slot-value as 'type) (file-types::file-mime (path as))
        (slot-value as 'data) (path as))
  (unless (destination as)
    (error "asset::initialize-instance: The destination for asset ~a is not ~
            set.")))


(defmethod print-object :before ((as asset) stream)
  (format stream "~%ASSET: path: ~a, ~
                    ~% description: ~a, ~% ~
                    destination: ~a, ~% ~
                    type: ~a"
          (path as) (description as) (destination as) (type as)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****f* asset/make-asset
;;; AUTHOR
;;; Ruben Philipp <me@rubenphilipp.com>
;;;
;;; CREATED
;;; 2023-07-23
;;; 
;;; DESCRIPTION
;;; This is a helper function to instantiate an asset object.
;;;
;;; ARGUMENTS
;;; - The path to the asset as a string.
;;; - The destination (i.e. the output path relative to the size asset
;;;   directory) of the asset as a string. 
;;; 
;;; OPTIONAL ARGUMENTS
;;; keyword-arguments:
;;; - :id. The id of the asset object. Default = NIL. 
;;; - :description. A short textual description of the asset. Must be a string.
;;;   Default = NIL. 
;;; 
;;; RETURN VALUE
;;; The asset object. 
;;;
;;; EXAMPLE
#|
(make-asset "~/screenshot1.png" "img/screenshot1.png")
|#
;;; SYNOPSIS
(defun make-asset (path destination
                   &key
                     (id nil)
                     (description nil))
  ;;; ****
  (make-instance 'asset
                 :id id
                 :path path
                 :destination destination
                 :description description))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; EOF asset.lisp
