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
;;; $$ Last modified:  08:56:45 Mon Jul 31 2023 CEST
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :colporter)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****f* utilities/trailing-slash
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
  (if (> (length path) 0)
    (if (char= #\/ (elt path (1- (length path))))
        path
        (format nil "~a/" path))
    ""))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****f* utilities/path-from-same-dir
;;; AUTHOR
;;; Ruben Philipp <me@rubenphilipp.com>
;;;
;;; CREATED
;;; 2023-07-09
;;; 
;;; DESCRIPTION
;;; This function returns the full path to a file reilative to the directory of
;;; the current lisp file.
;;; NB: This function is borrowed from Michael Edwards's slippery-chicken.
;;; NB2: This function does not work with files which have been loaded via
;;;      ASDF/quicklisp.
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
;;; ****f* utilities/path-from-src-dir
;;; AUTHOR
;;; Ruben Philipp <me@rubenphilipp.com>
;;;
;;; CREATED
;;; 2023-07-16
;;; 
;;; DESCRIPTION
;;; This function returns a path from the src dir. It is intended to be used
;;; within the main lisp files.
;;; NB: This function requires ASDF. 
;;;
;;; ARGUMENTS
;;; - A string indicating the filename (or pathname) to the file relative to
;;;   the src directory file.
;;; 
;;; RETURN VALUE
;;; A string with the full path to the file.
;;; 
;;; SYNOPSIS
(defun path-from-src-dir (file)
   ;;; ****
  (namestring (asdf::SYSTEM-RELATIVE-PATHNAME
               :klitter
               (concatenate 'string
                            "src/"
                            file))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****f* utilities/simple-shell
;;; AUTHOR
;;; Ruben Philipp <me@rubenphilipp.com>
;;;
;;; CREATED
;;; 2023-07-16
;;; 
;;; DESCRIPTION
;;; Run a shell command from lisp and return the exit code.
;;;
;;; ARGUMENTS
;;; - The shell command (i.e., most likely, path to the binary)
;;; 
;;; OPTIONAL ARGUMENTS:
;;; rest:
;;; - The arguments to the shell program.
;;; 
;;; RETURN VALUE
;;; The the exit-code of the process.
;;;
;;; SYNOPSIS
(defun simple-shell (command &rest arguments)
  ;;; ****
  (cl-user::process-exit-code
   (cl-user::run-program command arguments :output *standard-output*
                                           :wait t :input nil)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****f* utilities/shell
;;; AUTHOR
;;; Ruben Philipp <me@rubenphilipp.com>
;;;
;;; CREATED
;;; 2023-07-16
;;; 
;;; DESCRIPTION
;;; Runs a shell program and return the full result as a string or throws an
;;; error when the call to the program fails.
;;;
;;; ARGUMENTS
;;; - The command (e.g. a path to a binary).
;;; 
;;; OPTIONAL ARGUMENTS
;;; rest:
;;; - The arguments to the shell program. 
;;; 
;;; RETURN VALUE
;;; The result of the shell program call as a string.
;;;
;;; EXAMPLE
#|
(shell "git" "-v")
;; => "git version 2.40.1"
|#
;;; SYNOPSIS
(defun shell (command &rest arguments)
  ;;; ****
  (multiple-value-bind (output error-output exit-code)
      (uiop:run-program (cons command arguments)
                        :output :string
                        :error-output :string
                        :ignore-error-status t)
    (unless (zerop exit-code)
      (error "utilities::shell: The call to ~a failed. Error output: ~a ~%"
             command error-output))
    output))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****f* utilities/alistp
;;; AUTHOR
;;; Ruben Philipp <me@rubenphilipp.com>
;;;
;;; CREATED
;;; 2023-07-17
;;; 
;;; DESCRIPTION
;;; Tests if an object is of type alist.
;;;
;;; ARGUMENTS
;;; The object to test.
;;; 
;;; RETURN VALUE
;;; Either t or NIL
;;;
;;; EXAMPLE
#|
(alistp '((a . b)
          (c . d)))

;; => T
|#
;;; SYNOPSIS
(defun alistp (object)
  ;;; ****
  (and (listp object)
       (every #'consp object)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****f* utilities/assoc-keys
;;; AUTHOR
;;; Ruben Philipp <me@rubenphilipp.com>
;;;
;;; CREATED
;;; 2023-07-17
;;; 
;;; DESCRIPTION
;;; Returns a list with all keys of an alist. 
;;;
;;; ARGUMENTS
;;; An alist. 
;;; 
;;; RETURN VALUE
;;; A list with keys of the alist.
;;;
;;; EXAMPLE
#|
(let ((lst '((:test . 12)
             (:value2 . 13)
             (:something . 'of-importance))))
  (assoc-keys lst))

;; => '(:TEST :VALUE2 :SOMETHING)
|#
;;; SYNOPSIS
(defun assoc-keys (alist)
  ;;; ****
  ;; sanity checks
  (unless (alistp alist)
    (error "utilities::assoc-keys: The value is not of type alist."))
  (loop for item in alist
        collect
        (car item)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****f* utilities/uid-from-path
;;; AUTHOR
;;; Ruben Philipp <me@rubenphilipp.com>
;;;
;;; CREATED
;;; 2023-07-24
;;; 
;;; DESCRIPTION
;;; This function returns a uid from a given path.
;;; When base is set, the uid will be relative to the path given as base.
;;;
;;; ARGUMENTS
;;; A path to a file as a string. 
;;; 
;;; OPTIONAL ARGUMENTS
;;; - base. The root directory, which serves as the base for retrieving a
;;;   (relative) UID. Either a string or NIL. Default = NIL. 
;;; 
;;; RETURN VALUE
;;; The UID generated. As a string.
;;;
;;; EXAMPLE
#|
(uid-from-path "/sites/rubenphilipp/content/projects/opus-1.yaml"
               "/sites/rubenphilipp/content/")

;; => "projects/opus-1"
|#
;;; SYNOPSIS
(defun uid-from-path (path &optional base)
  ;;; ****
  ;; ensure that trailing slash is used
  (setf base (trailing-slash base))
  (if base
      (setf path (enough-namestring path base))
      (setf path (enough-namestring path "/")))
  (let ((uid-elements (cdr (pathname-directory (directory-namestring path)))))
    (loop for i from 0 to (length uid-elements)
          for uid-e = (nth i uid-elements)
          with result = ""
          if (= i (length uid-elements))
            do (setf result
                     (concatenate 'string result (pathname-name path)))
          else
            do (setf result
                     (concatenate 'string result uid-e "/"))
          finally
             (return result))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****f* utilities/leading-slash
;;; AUTHOR
;;; Ruben Philipp <me@rubenphilipp.com>
;;;
;;; CREATED
;;; 2023-07-31
;;; 
;;; DESCRIPTION
;;; This function ensures that a string starts with a slash (e.g. useful when
;;; treating relative as absolute paths).
;;;
;;; ARGUMENTS
;;; A string.
;;;
;;; OPTIONAL ARGUMENTS
;;; keyword-arguments:
;;; - :dot. When T, also adds a "." to the leading path (i.e. "./").
;;;   Default = NIL.
;;; 
;;; RETURN VALUE
;;; The string with a leading slash.
;;;
;;; EXAMPLE
#|
(leading-slash "hello/world")
;; => "/hello/world"
|#
;;; SYNOPSIS
(defun leading-slash (string &key dot)
  ;;; ****
  (let ((result 
          (if (or (string= "/" (subseq string 0 1))
                  (string= "./" (subseq string 0 2)))
              string
              (concatenate 'string "/" string))))
    (if dot
        (concatenate 'string "." result)
        result)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****f* utilities/relative-path
;;; AUTHOR
;;; Ruben Philipp <me@rubenphilipp.com>
;;;
;;; CREATED
;;; 2023-07-30
;;; 
;;; DESCRIPTION
;;; Returns a path to the file indicated by the target path relative to the
;;; location directory.
;;; E.g.:
;;; location: "/tmp/project/"
;;; target: "/tmp/image.jpg"
;;; => result: "../image.jpg"
;;;
;;; ARGUMENTS
;;; - The location directory from which the relative link to the target has to
;;;   be constructed. Must be a string.
;;; - The target path (e.g. to a file). 
;;; 
;;; RETURN VALUE
;;; A pathname namestring being the path to the target relative to the location.
;;;
;;; EXAMPLE
#|
(let ((ln "tmp/project/")
      (tg "tmp/images/test.jpg"))
  (relative-path ln tg))
;; => "../images/test.jpg"
|#
;;; SYNOPSIS
(defun relative-path (location target)
  ;;; ****
  ;;; when location is empty / nil or "/", assume that it is the root / base
  ;;; RP  Mon Jul 31 07:51:56 2023
  (if (eq nil (pathname-directory (trailing-slash location)))
      target
      (let* ((loc (trailing-slash (leading-slash location :dot t)))
             (targ (leading-slash target :dot t))
             (result 
              (loop for ln on (pathname-directory loc)
                    for tg on (pathname-directory targ)
                    while (string= (first ln) (first tg))
                    finally
                       (return
                         (make-pathname
                          :directory (append (list :relative)
                                             (substitute :up t ln :test
                                                         (constantly t))
                                             tg)
                          :defaults targ)))))
        (namestring result))))
        



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; EOF utilities.lisp
