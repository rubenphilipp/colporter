;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; FILE
;;; utilities.lisp
;;;
;;; NAME
;;; utilities
;;;
;;; DESCRIPTION
;;; Utility functions for colporter.
;;;
;;; AUTHOR
;;; Ruben Philipp <me@rubenphilipp.com>
;;;
;;; CREATED
;;; 2023-07-09
;;;
;;; $$ Last modified:  22:26:37 Sun Jul  9 2023 CEST
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :colporter)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****f* trailing-slash
;;; AUTHOR
;;; Ruben Philipp <me@rubenphilipp.com>
;;;
;;; CREATED
;;; 2023-07-09
;;; 
;;; DESCRIPTION
;;; This function ensures that a (path) string ends with a trailing slash.
;;; NB: This function is borrowed from Michael Edward's slippery-chicken.
;;;
;;; ARGUMENTS
;;; A string containing the path to be checked and corrected.
;;; 
;;; RETURN VALUE
;;; The path with a trailing slash.
;;;
;;; SYNOPSIS
(defun trailing-slash (path)
  ;;; ****
  (when (> (length path) 0)
    (if (char= #\/ (elt path (1- (length path))))
        path
        (format nil "~a/" path))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****f* path-from-same-dir
;;; AUTHOR
;;; Ruben Philipp <me@rubenphilipp.com>
;;;
;;; CREATED
;;; 2023-07-09
;;; 
;;; DESCRIPTION
;;; This function returns the full path to a file reilative to the directory of
;;; the current lisp file.
;;; NB: This function is borrowed from Michael Edward's slippery-chicken.
;;;
;;; ARGUMENTS
;;; - A string indicating the filename (or pathname) to the file relative to
;;;   the current lisp file.
;;; 
;;; RETURN VALUE
;;; A string with the full path to the file.
;;; 
;;; SYNOPSIS
 (defun path-from-same-dir (file)
   ;;; ****
  (concatenate 'string
               (trailing-slash
                (directory-namestring (truename *load-pathname*)))
               file))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; EOF utilities.lisp
