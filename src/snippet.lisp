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
;;; $$ Last modified:  23:49:56 Tue Aug  1 2023 CEST
;;; ****
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :colporter)

(defclass snippet (named-object)
  ;; a short (optional) documentation string of the snippet
  ((description :accessor description :initarg :description :initform nil)
   ;; the snippet (must be a function which takes any arbitrary amount of
   ;; arguments)
   (snippet :accessor snippet :initarg :snippet :initform nil)))

(defmethod initialize-instance :after ((sn snippet) &rest initargs)
  (declare (ignore initargs))
  (unless (functionp (snippet sn))
    (error "snippet::initialize-instance: The snippet must be of type ~
            FUNCTION, not ~a."
           (type-of (snippet sn))))
  (setf (slot-value sn 'data) (snippet sn)))


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
;;; The snippet. Must be a (lambda) function which takes an arbitrary amount
;;; of arguments.
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
(defun make-snippet (snippet &key
                                   (description "")
                                   (id nil))
  ;;; ****
  (make-instance 'snippet
                 :snippet snippet
                 :description description
                 :id id))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****** snippet/define-snippet
;;; AUTHOR
;;; Ruben Philipp <me@rubenphilipp.com>
;;;
;;; CREATED
;;; 2023-07-24
;;; 
;;; DESCRIPTION
;;; This macro expands to a function definition to be used to create snippet
;;; functions. 
;;;
;;; ARGUMENTS
;;; - The arguments to the snippet function (can be used in the body).
;;; - The body of the snippet function. 
;;;
;;; EXAMPLE
#|
(funcall (define-snippet (title text)
           (with-html-string
             (:h1 title)
             (:p text))) "test" "Ein Text...")
;; => "<h1>test</h1>
;;     <p>Ein Text..."
|#
;;; SYNOPSIS
(defmacro define-snippet ((&rest args) &body body)
  ;;; ****
  `(lambda (,@args)
     ,@body))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****m* snippet/do-snippet
;;; AUTHOR
;;; Ruben Philipp <me@rubenphilipp.com>
;;;
;;; CREATED
;;; 2023-07-23
;;; 
;;; DESCRIPTION
;;; This method evaluates the snippet with the given arguments.
;;;
;;; ARGUMENTS
;;; - A snippet object.
;;;
;;; OPTIONAL ARGUMENTS
;;; rest:
;;; - The arguments to the snippet.
;;; 
;;; RETURN VALUE
;;; The return value of the snippet.
;;;
;;; EXAMPLE
#|
(let ((sn (make-snippet #'(lambda (x y) (+ x y)))))
  (do-snippet sn 4 5))
|#
;; => 9
;;; SYNOPSIS
(defmethod do-snippet ((sn snippet) &rest args)
  ;;; ****
  (apply (snippet sn) args))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; EOF snippet.lisp
