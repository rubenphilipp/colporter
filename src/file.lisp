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
;;; $$ Last modified:  15:24:52 Mon Jul 24 2023 CEST
;;; ****
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :colporter)

(defclass file (named-object)
  ;; the (absolute) path to the file
  ((path :accessor path :initarg :path :initform nil)
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
  (format stream "~%FILE: path: ~a, description: ~a, ~
                  size (KB): ~a, type: ~a"
          (path fl) (description fl) (size fl) (type fl)))

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
  (let* ((file-stats (osicat-posix:stat (path fl)))
         (file-size-kb (/ (osicat-posix:stat-size file-stats) 1000.0))
         (file-mime (file-types::file-mime (path fl))))
    (setf (slot-value fl 'size) file-size-kb
          (slot-value fl 'type) file-mime)))


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
;;; The path to the file. 
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
(let ((file (make-file "~/image.jpg")))
  (type file))
;; => ("imace" "jpeg")
|#
;;; SYNOPSIS
(defun make-file (path &key
                         (description "")
                         (id nil))
  ;;; ****
  (make-instance 'file :path path
                       :description description
                       :id id))


   

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; EOF file.lisp
