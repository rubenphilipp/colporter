;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****c* file/asset
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
;;; be stored in asset objects (cf. file). 
;;;
;;; CLASS HIERARCHY
;;; named-object -> file -> asset
;;;
;;; $$ Last modified:  17:05:31 Mon Jul 24 2023 CEST
;;; ****
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :colporter)

(defclass asset (file)
  ;; the UID (unique id) of the asset
  ;; this is most related to the output path relative to the site asset
  ;; directory (e.g. "css/main" => "css/main.css")
  ;; all other slots are inherited from the file class
  ;; RP  Mon Jul 24 15:27:31 2023
  ((uid :accessor uid :initarg :uid :initform nil)))

(defmethod initialize-instance :after ((as asset) &rest initargs)
  (declare (ignore initargs))
  (unless (uid as)
    (error "asset::initialize-instance: The uid for asset ~a is not ~
            set.")))


(defmethod print-object :before ((as asset) stream)
  (format stream "~%ASSET: uid: ~a"
          (uid as)))

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
;;; - The uid (i.e. most likely the output path relative to the size asset
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
(defun make-asset (path uid
                   &key
                     (id nil)
                     (description nil))
  ;;; ****
  (make-instance 'asset
                 :id id
                 :path path
                 :uid uid
                 :description description))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; EOF asset.lisp
