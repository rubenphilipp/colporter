;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****c* named-object/file
;;; NAME
;;; file
;;; 
;;; AUTHOR
;;; Ruben Philipp <me@rubenphilipp.com>
;;;
;;; CREATED
;;; 2023-07-24
;;; 
;;; PURPOSE
;;; Implementation of the file class and related methods. This class holds
;;; general information (e.g. size, mime type etc.) about files (which could be
;;; for example files associated with a page, e.g. images, or assets -- cf.
;;; asset). Nonetheless, file objects just contain references to files but
;;; not the data / the file itself.
;;;
;;; CLASS HIERARCHY
;;; named-object -> file
;;;
;;; $$ Last modified:  15:14:21 Tue Jul 25 2023 CEST
;;; ****
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :colporter)

(defclass file (named-object)
  ;; the (absolute) path to the file
  ((path :accessor path :initarg :path :initform nil)
   ;; the namestring of the file
   (filename :accessor filename :initform nil)
   ;; the file extension
   (extension :accessor extension :initform nil)
   ;; the uid of the file
   (uid :accessor uid :initarg :uid :initform nil)
   ;; the base path of the file, used for generating the uid
   (base :accessor base :initarg :base :initform nil)
   ;; a simple (optional) textual description of the file
   (description :accessor description :initarg :description :initform "")
   ;; the size of the the file (in kilobytes)
   (size :accessor size :initform nil)
   ;; the file mime type (retrieved via file-types::file-mime)
   (type :accessor type :initform nil)))

(defmethod initialize-instance :after ((fl file) &rest initargs)
  (declare (ignore initargs))
  (unless (probe-file (path fl))
    (error "file::initialize-instance: The file ~a does not exist. "
           (path fl)))
  (update fl))

(defmethod print-object :before ((fl file) stream)
  (format stream "~%FILE: path: ~a, uid: ~a ~%~
                  description: ~a, extension: ~a ~
                  size (KB): ~a, type: ~a"
          (path fl) (uid fl) (description fl) (extension fl)
          (size fl) (type fl)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod (setf path) :after (value (fl file))
  (declare (ignore value))
  (update fl))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****m* file/update
;;; AUTHOR
;;; Ruben Philipp <me@rubenphilipp.com>
;;;
;;; CREATED
;;; 2023-07-24
;;; 
;;; DESCRIPTION
;;; This method updates the slots in a file-object. 
;;;
;;; ARGUMENTS
;;; A file object. 
;;; 
;;; RETURN VALUE
;;; The updated file object. 
;;;
;;; SYNOPSIS
(defmethod update ((fl file))
  ;;; ****
  (unless (uid fl)
    (error "file::update: No uid is set."))
  (let* ((file-stats (osicat-posix:stat (path fl)))
         (file-size-kb (/ (osicat-posix:stat-size file-stats) 1000.0))
         (file-mime (file-types::file-mime (path fl))))
    (setf (slot-value fl 'size) file-size-kb
          (slot-value fl 'extension) (pathname-type (path fl))
          (slot-value fl 'filename) (file-namestring (path fl))
          (slot-value fl 'type) file-mime
          (slot-value fl 'data) (uid fl)))
  ;; generate uid
  ;; (unless (uid fl)
  ;;   (setf (slot-value fl 'uid)
  ;;         (uid-from-path (path fl) (base fl))))
  ;; set id according to uid
  (setf (slot-value fl 'id) (uid fl))
  fl)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****f* file/make-file
;;; AUTHOR
;;; Ruben Philipp <me@rubenphilipp.com>
;;;
;;; CREATED
;;; 2023-07-24
;;; 
;;; DESCRIPTION
;;; Helper function to make a file object. 
;;;
;;; ARGUMENTS
;;; - The path to the file.
;;; - The uid of the file. This should be a string mirroring the path
;;;   to the file relative to the site base path (cf. colporter), as this will
;;;   be used both for referencing the file and for generating the url which
;;;   will be used e.g. in html output. For example, a file with the path
;;;   "/contents/projects/image.jpg" should obtain the uid "projects/image.jpg".
;;; 
;;; OPTIONAL ARGUMENTS
;;; keyword-arguments:
;;; - :description. An optional short description as a string. Default = "".
;;; - :id. The id of the file. Default = NIL.
;;; 
;;; RETURN VALUE
;;; The file object. 
;;;
;;; EXAMPLE
#|
(let ((file (make-file "image.jpg" "~/image.jpg")))
  (type file))
;; => ("image" "jpeg")
|#
;;; SYNOPSIS
(defun make-file (path uid &key
                             (description "")
                             (id nil))
  ;;; ****
  (make-instance 'file :path path
                       :uid uid
                       :description description
                       :id id))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****m* file/imagep
;;; AUTHOR
;;; Ruben Philipp <me@rubenphilipp.com>
;;;
;;; CREATED
;;; 2023-07-24
;;; 
;;; DESCRIPTION
;;; Tests if the file is an image. 
;;;
;;; ARGUMENTS
;;; A file object. 
;;; 
;;; RETURN VALUE
;;; Either T or NIL. 
;;;
;;; EXAMPLE
#|
(let ((file (make-file "~/test.jpg")))
  (imagep file))
;; => T
|#
;;; SYNOPSIS
(defmethod imagep ((fl file))
  ;;; ****
  (equal (first (type fl)) "image"))


   

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; EOF file.lisp
