;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****c* named-object/snippet
;;; NAME
;;; snippet
;;; 
;;; AUTHOR
;;; Ruben Philipp <me@rubenphilipp.com>
;;;
;;; CREATED
;;; 2023-07-23
;;; 
;;; PURPOSE
;;; Implementation of the snippet class and related methods and functions.
;;; A snippet is essentially a function which takes an arbitrary amount of
;;; arguments and returns a certain value (e.g. HTML, spinneret "markup" etc.).
;;;
;;; CLASS HIERARCHY
;;; named-object -> snippet
;;;
;;; $$ Last modified:  23:52:21 Sun Jul 23 2023 CEST
;;; ****
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :colporter)

(defclass snippet (named-object)
  ;; a short (optional) documentation string of the snippet-fun
  ((description :accessor description :initarg :description :initform nil)
   ;; the snippet fun (e.g. lambda function)
   (snippet-fun :accessor snippet-fun :initarg :snippet-fun :initform nil)))

(defmethod initialize-instance :after ((sn snippet) &rest initargs)
  (declare (ignore initargs))
  (unless (functionp (snippet-fun sn))
    (error "snippet::initialize-instance: The snippet-fun must be of type ~
            FUNCTION, not ~a."
           (type-of (snippet-fun sn))))
  (setf (slot-value sn 'data) (snippet-fun sn)))


(defmethod print-object :before ((sn snippet) stream)
  (format stream "~%SNIPPET: description: ~a"
          (description sn)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****f* snippet/make-snippet
;;; AUTHOR
;;; Ruben Philipp <me@rubenphilipp.com>
;;;
;;; CREATED
;;; 2023-07-23
;;; 
;;; DESCRIPTION
;;; This function is a shorthand to instantiate a snippet object. 
;;;
;;; ARGUMENTS
;;; The snippet function. Must be a (lambda) function.
;;; 
;;; OPTIONAL ARGUMENTS
;;; keyword-arguments:
;;; - :description. A short description of the lambda functions (preferably
;;;   including its arguments) as a string.
;;; - :id. The id of the snippet-object.
;;; 
;;; RETURN VALUE
;;; The snippet-object. 
;;;
;;; EXAMPLE
#|
(funcall (data (make-snippet #'(lambda (x) (+ x 3))
:description "Add 3 to x.")) 3)
;; => 6
|#
;;; SYNOPSIS
(defun make-snippet (snippet-fun &key
                                   (description "")
                                   (id nil))
  ;;; ****
  (make-instance 'snippet
                 :snippet-fun snippet-fun
                 :description description
                 :id id))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****m* snippet/snippet
;;; AUTHOR
;;; Ruben Philipp <me@rubenphilipp.com>
;;;
;;; CREATED
;;; 2023-07-23
;;; 
;;; DESCRIPTION
;;; This method evaluates the snippet-fun with the given arguments.
;;;
;;; ARGUMENTS
;;; - A snippet object.
;;; - The arguments to the snippet-fun.
;;; 
;;; RETURN VALUE
;;; The return value of the snippet-fun.
;;;
;;; EXAMPLE
#|
(let ((sn (make-snippet #'(lambda (x y) (+ x y)))))
  (snippet sn 4 5))
|#
;; => 9
;;; SYNOPSIS
(defmethod snippet ((sn snippet) &rest args)
  ;;; ****
  (apply (snippet-fun sn) args))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; EOF snippet.lisp
