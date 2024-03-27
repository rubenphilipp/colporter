;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; FILE
;;; utilities.lisp
;;;
;;; NAME
;;; utilities
;;;
;;; DESCRIPTION
;;; Utilities relevant for Ruben Philipp's website. 
;;;
;;; AUTHOR
;;; Ruben Philipp <me@rubenphilipp.com>
;;;
;;; CREATED
;;; 2023-07-28
;;;
;;; $$ Last modified:  23:37:27 Tue Aug  1 2023 CEST
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :rubenphilipp)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; load all files in a directory by a certain extension

(defmacro load-files-by-extension (dir extension)
  `(let ((files (uiop:directory-files ,dir
                                      (concatenate
                                       'string
                                       "*."
                                       (if (equal "."
                                                  (subseq ,extension 0 1))
                                           (subseq ,extension 1)
                                           ,extension)))))
     (loop for file in files
           do
              (load file))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; add a snippet to +rp-snippets+
;;; RP  Tue Aug  1 22:41:10 2023
#|
(add-snippet ("header" title)
  (with-html
    (:head
     (:title title))))
|#

(defmacro add-snippet ((id &rest args) &body body)
  `(setf (gethash ,id rp-web::+rp-snippets+)
         (colporter::make-snippet
          (colporter::define-snippet (,@args)
            ,@body)
          :id ,id)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; add a template to +rp-templates+
;;; RP  Tue Aug  1 23:01:14 2023
#|
(add-template ("default")
  (:body
   (:p "hallo!")))
|#

(defmacro add-template ((id) &body body)
  `(setf (gethash ,id rp-web::+rp-templates+)
         (colporter::make-template
          (colporter::define-template
            ,@body)
          :id ,id)))
  

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; EOF utilities.lisp
