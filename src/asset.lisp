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
;;; $$ Last modified:  15:18:53 Tue Jul 25 2023 CEST
;;; ****
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :colporter)

(defclass asset (file)
  ;; nothing to add
  ())

(defmethod initialize-instance :after ((as asset) &rest initargs)
  (declare (ignore initargs))
  (unless (uid as)
    (error "asset::initialize-instance: The uid for asset ~a is not ~
            set.")))


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
;;; - The uid of the asset. This should be a string mirroring the path
;;;   to the asset relative to the site asset path (cf. site and colporter), as
;;;   this will  be used both for referencing the asset and for generating the
;;;   url which will be used e.g. in html output. For example, an asset with
;;;   the path "/assets/css/main.css" should obtain the uid
;;;   "css/main.css".
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
