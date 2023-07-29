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
;;; $$ Last modified:  16:33:51 Sat Jul 29 2023 CEST
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
;;; EOF utilities.lisp
