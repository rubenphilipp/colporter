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
;;; $$ Last modified:  19:06:32 Mon Mar 18 2024 CET
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
            set." (type-of as))))


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
;;; ****f* asset/make-assets-from-dir
;;; AUTHOR
;;; Ruben Philipp <me@rubenphilipp.com>
;;;
;;; CREATED
;;; 2023-07-24
;;; 
;;; DESCRIPTION
;;; This function recursively scans a given directory for all files contained
;;; therein and instantiates file objects with the respective uid set according
;;; to the root of the given path. Finally, the generated objects will be
;;; returned as a hash-table with keys set to the uids to be further used in a
;;; site object. 
;;;
;;; ARGUMENTS
;;; A string being the path to the directory to recursively scan for assets.
;;; 
;;; RETURN VALUE
;;; A hash-table with asset objects (see above and cf. site).
;;;
;;; EXAMPLE
#|
(let ((ass (make-assets-from-dir
            "/Users/rubenphilipp/lisp/colporter/tests/assets")))
  (hash-table-keys ass))
;; => ("projects/bla/dingsdo.css" "projects/testb.jpg" "test.jpg")
|#
;;; SYNOPSIS
(defun make-assets-from-dir (dir)
  ;;; ****
  (let ((dir (trailing-slash dir))
        (file-paths nil)
        (assets (make-hash-table :test 'equal)))
    ;; scan assets
    (cl-fad:walk-directory dir
                           #'(lambda (name)
                               (when ;; do not include hidden files
                                      (not (equal "."
                                                  (subseq
                                                   (file-namestring name)
                                                   0 1)))
                                 (push name file-paths)))
                           :directories nil)
    ;; generate file objects
    (loop for path in file-paths
          for uid = (enough-namestring path dir)
          for file = (make-file path uid :id uid)
          do
             (setf (gethash uid assets) file))
    assets))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; EOF asset.lisp
