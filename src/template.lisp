;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****c* named-object/template
;;; NAME
;;; template
;;; 
;;; AUTHOR
;;; Ruben Philipp <me@rubenphilipp.com>
;;;
;;; CREATED
;;; 2023-07-23
;;; 
;;; PURPOSE
;;; Implementation of the template class and related methods and functions.
;;; A template consists of a function which takes a page and a site object as
;;; its arguments and returns a string which will be stored as the page data
;;; (thus, most likely, containing HTML markup).
;;;
;;; CLASS HIERARCHY
;;; named-object -> template
;;;
;;; $$ Last modified:  15:28:01 Wed Oct 18 2023 CEST
;;; ****
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :colporter)

(defclass template (named-object)
  ;; a short (optional) description of the template
  ((description :accessor description :initarg :description :initform nil)
   ;; the template function (e.g. a lambda function), for details see above
   (template :accessor template :initarg :template :initform nil)))


(defmethod initialize-instance :after ((tp template) &rest initargs)
  (declare (ignore initargs))
  (unless (functionp (template tp))
    (error "template::initialize-instance: The template must be of type ~
            FUNCTION, not ~a."
           (type-of (template tp))))
  (setf (slot-value tp 'data) (template tp)))

(defmethod print-object :before ((tp template) stream)
  (format stream "~%TEMPLATE: description: ~a"
          (description tp)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****f* template/make-template
;;; AUTHOR
;;; Ruben Philipp <me@rubenphilipp.com>
;;;
;;; CREATED
;;; 2023-07-23
;;; 
;;; DESCRIPTION
;;; This function is a shorthand to instantiate a template object.
;;;
;;; ARGUMENTS
;;; The template (function). Must be a (lambda) function. This function must 
;;; take at least two arguments:
;;; - A page object.
;;; - A site object. 
;;; 
;;; OPTIONAL ARGUMENTS
;;; keyword-arguments:
;;; - :description. A short description of the template function as a string.
;;; - :id. The id of the template object. 
;;;
;;; RETURN VALUE
;;; The template object. 
;;; 
;;; SYNOPSIS
(defun make-template (template
                      &key
                        (description "")
                        (id nil))
  ;;; ****
  (make-instance 'template
                 :template template
                 :description description
                 :id id))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****** template/define-template
;;; AUTHOR
;;; Ruben Philipp <me@rubenphilipp.com>
;;;
;;; CREATED
;;; 2023-07-24
;;; 
;;; DESCRIPTION
;;; This macro expands to a function definition to be used to create a template
;;; function (i.e. a function which takes a page and a site as arguments).
;;; 
;;; As the aforementioned objects are available when the template function is
;;; evaluated during the context of assembling a colporter-site, all related
;;; methods and the slots of page and site objects (via the `site` and `page`
;;; symbols are available in the body of the template function. 
;;;
;;; ARGUMENTS
;;; - The body of the template (function). 
;;; 
;;; RETURN VALUE
;;; Expands to a lambda function. 
;;;
;;; EXAMPLE
#|
(define-template
  (with-html
    (:p (content page))))

;; => (LAMBDA (SITE PAGE)
;;     (EVAL
;;      (WITH-HTML
;;        (:P (CONTENT PAGE)))))
|#
;;; SYNOPSIS
(defmacro define-template (&body body)
  ;;; ****
  `(lambda (page site)
     ,@body))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****m* template/do-template
;;; AUTHOR
;;; Ruben Philipp <me@rubenphilipp.com>
;;;
;;; CREATED
;;; 2023-07-24
;;; 
;;; DESCRIPTION
;;; This method evaluates the template with the given page and site object. 
;;;
;;; ARGUMENTS
;;; - The template object.
;;; - A page object.
;;; - A site object. 
;;; 
;;; RETURN VALUE
;;; The return value of the template. 
;;;
;;; SYNOPSIS
(defmethod do-template ((template template) page site)
  ;;; ****
  ;; sanity checks
  (unless (typep page 'page)
    (error "template::do-template: The page must be of type PAGE, not ~a."
           (type-of page)))
  (unless (typep site 'site)
    (error "template::do-template: The site must be of type SITE, not ~a."
           (type-of site)))
  (funcall (template template) page site))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****** template/yield-snippet
;;; AUTHOR
;;; Ruben Philipp <me@rubenphilipp.com>
;;;
;;; CREATED
;;; 2023-07-24
;;; 
;;; DESCRIPTION
;;; This macro can be used within define-template in order to retrieve a 
;;; snippet object available to the (implicitly given) site object by solely
;;; referring to the id by which the respective snippet object is stored in
;;; the snippets slot of the site object. 
;;;
;;; ARGUMENTS
;;; - The snippet id (see above).
;;;
;;; EXAMPLE
#|
(yield-snippet "sn1")
;; => (GET-SNIPPET SITE "sn1")
|#
;;; SYNOPSIS
(defmacro yield-snippet (id)
  ;;; ****
  `(get-snippet site ,id))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****** template/insert-snippet
;;; AUTHOR
;;; Ruben Philipp <me@rubenphilipp.com>
;;;
;;; CREATED
;;; 2023-07-24
;;; 
;;; DESCRIPTION
;;; This macro can be used within define-template in order to insert a snippet
;;; available to the (implicitly given) site object by solely referring to
;;; the id by which the respective snippet object is stored in the snippets
;;; slot of the site object. 
;;;
;;; ARGUMENTS
;;; - The snippet id (see above). 
;;; 
;;; OPTIONAL ARGUMENTS
;;; rest:
;;; - All other arguments required by the snippet according to its definition
;;;   (cf. define-snippet). 
;;; 
;;; EXAMPLE
#|
(insert-snippet "sn1" 1 'test)
;; =>
;; (LET ((SNIPPET (GET-SNIPPET SITE "sn1")))
;;   (APPLY #'DO-SNIPPET (LIST "sn1" 1 'TEST)))
|#
;;; SYNOPSIS
(defmacro insert-snippet (id &rest args)
  ;;; ****
  `(let ((snippet (get-snippet site ,id)))
     (apply #'do-snippet (list snippet ,@args))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****** template/yield-asset
;;; AUTHOR
;;; Ruben Philipp <me@rubenphilipp.com>
;;;
;;; CREATED
;;; 2023-07-24
;;; 
;;; DESCRIPTION
;;; This macro can be used within define-template in order to retrieve a 
;;; asset object available to the (implicitly given) site object by solely
;;; referring to the id by which the respective asset object is stored in
;;; the assets slot of the site object. 
;;;
;;; ARGUMENTS
;;; - The asset id (see above).
;;;
;;; EXAMPLE
#|
(yield-asset "style.css")
;; => (GET-ASSET SITE "sn1")
|#
;;; SYNOPSIS
(defmacro yield-asset (id)
  ;;; ****
  `(get-asset site ,id))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****** template/insert-asset-path
;;; AUTHOR
;;; Ruben Philipp <me@rubenphilipp.com>
;;;
;;; CREATED
;;; 2023-07-24
;;; 
;;; DESCRIPTION
;;; This macro inserts the asset path relative to the asset base dir (cf.
;;; site) which could be used for referencing to the file in an html-context
;;; (e.g. in the href for images). This macro is intended to be used in the
;;; context of define-template.
;;;
;;; ARGUMENTS
;;; The asset id (see above).
;;; 
;;; EXAMPLE
#|
(insert-asset-path "css/main.css")
;; =>
;; (LET ((ASSET (GET-ASSET SITE "css/main.css"))
;;       (ASSET-BASE-DIR (ASSET-BASE-DIR SITE)))
;;   (CONCATENATE 'STRING ASSET-BASE-DIR (DATA ASSET)))
|#
;;; SYNOPSIS
(defmacro insert-asset-path (id)
  ;;; ****
  `(let ((asset (get-asset site ,id))
         (asset-base-dir (asset-base-dir site))
         (page-location (uid page)))
     (relative-path
      (directory-namestring page-location)
      (concatenate 'string
                   asset-base-dir
                   (data asset)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****** template/yield-page-by-uuid
;;; AUTHOR
;;; Ruben Philipp <me@rubenphilipp.com>
;;;
;;; CREATED
;;; 2023-07-24
;;; 
;;; DESCRIPTION
;;; This macro can be used within define-template in order to retrieve a 
;;; page object available to the (implicitly given) site object by solely
;;; referring to the uuid by which the respective asset object is stored in
;;; the assets slot of the site object. 
;;;
;;; ARGUMENTS
;;; - The asset id (see above).
;;;
;;; EXAMPLE
#|
(yield-page-by-uuid "778425df-7450-4a18-b58b-9448f1de9b7a")
;; => (GET-PAGE-BY-UUID SITE "778425df-7450-4a18-b58b-9448f1de9b7a")
|#
;;; SYNOPSIS
(defmacro yield-page-by-uuid (uuid)
  ;;; ****
  `(get-page-by-uuid site ,uuid))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****** template/insert-page-path-by-uuid
;;; AUTHOR
;;; Ruben Philipp <me@rubenphilipp.com>
;;;
;;; CREATED
;;; 2023-07-31
;;; 
;;; DESCRIPTION
;;; This macro can be used within define-template in order to insert the
;;; path to a page by its uuid.
;;; The retrieved path will be absolute to the document root.
;;;
;;; ARGUMENTS
;;; - The uuid of the page to insert. 
;;;
;;; EXAMPLE
#|
(insert-page-path-by-uuid "778425df-7450-4a18-b58b-9448f1de9b7a")
;; =>
;; (LET ((PAGE-TO-INSERT
;;        (GET-PAGE-BY-UUID SITE "778425df-7450-4a18-b58b-9448f1de9b7a"))
;;       (THIS-PAGE-LOCATION (UID PAGE)))
;;   (RELATIVE-PATH (DIRECTORY-NAMESTRING THIS-PAGE-LOCATION)
;;               (UID PAGE-TO-INSERT)))
|#
;;; SYNOPSIS
(defmacro insert-page-path-by-uuid (uuid)
  ;;; ****
  `(let ((page-to-insert (get-page-by-uuid site ,uuid))
         (this-page-location (uid page)))
     (concatenate 'string
                  "/"
                  (uid page-to-insert))))
;;; changed to make path absolute
;;; RP  Wed Oct 18 15:27:45 2023
     ;; (relative-path
     ;;  (directory-namestring this-page-location)
     ;;  (uid page-to-insert))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; RP  Sat Aug  5 23:40:46 2023
;;; returns an absolute path as of 2023-10-18.

(defmacro insert-page-path (uid)
  `(concatenate 'string
                "/"
                ,uid))
;;; changed to make path absolute
;;; RP  Wed Oct 18 15:28:00 2023
  ;; `(relative-path (directory-namestring
  ;;                  (uid page))
  ;;                 ,uid))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****** template/insert-file-path
;;; AUTHOR
;;; Ruben Philipp <me@rubenphilipp.com>
;;;
;;; CREATED
;;; 2023-07-24
;;; 
;;; DESCRIPTION
;;; This macro inserts the path of a file object, relative to the current
;;; page object. 
;;; NB: This macro is intended to be used in the context of define-template.
;;;     An object with the symbol page must be available in the lexical context.
;;;
;;; NB2: As of 2023-10-18, this macro returns an absolute path to the document
;;;      root. 
;;;
;;; ARGUMENTS
;;; The (relative or absolute) uid of the file object, as referenced in the
;;; site object hash-table. 
;;; 
;;; OPTIONAL ARGUMENTS
;;; - A boolean indicating whether the given uid path is relative of the current
;;;   page or relative (i.e. "absolute") to the site base path.
;;;   Examples:
;;;   A) relative:
;;;      page-uid: "projects/opus-1"
;;;      actual file-uid: "projects/images/test.jpg"
;;;      file-uid used as argument: "images/test.jpg"
;;;      => will resolve into the actual file-uid
;;;   B) absolute:
;;;      in this case the actual file-uid corresponds the file-uid given as
;;;      argument.
;;;   Default = T (i.e. relative).
;;; 
;;; EXAMPLE
#|
(insert-file-path "images/test.jpg")
;; =>
;; (LET ((FILE-ID
;;        (CONCATENATE 'STRING
;;                     (DIRECTORY-NAMESTRING (UID PAGE) "images/test.jpg"))))
;;   (DATA (GET-FILE SITE FILE-ID)))
|#
;;; SYNOPSIS
(defmacro insert-file-path (uid &optional (relative t))
  ;;; ****
  (if relative
      `(let ((file-id (concatenate 'string
                                   (directory-namestring
                                    (uid page))
                                   ,uid)))
         (concatenate 'string
                      "/"
                      (data (get-file site file-id))))
      `(concatenate 'string
                         "/"
                         (data (get-file site ,uid)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****** template/with-colportage
;;; AUTHOR
;;; Ruben Philipp <me@rubenphilipp.com>
;;;
;;; CREATED
;;; 2023-07-24
;;; 
;;; DESCRIPTION
;;; This macro -- to be used in the context of define-template -- is part of the
;;; implementation of colportage, a markdown-variant which implements
;;; features specific to colporter. Its main purpose is to make snippets and
;;; files available in the page source files (e.g. the YAML files used to
;;; generate page objects).
;;; Strings evaluated via with-colportage will go through the following steps:
;;; 1) The string will be parsed via parse-markdown in order to convert
;;;    markdown to html syntax.
;;; 2) Then, all colportage-specific tags (e.g. [[file uid]] or [[asset ...]])
;;;    will be converted. In this conversion process, the path to the actual
;;;    file or snippet (in this example) will be retrieved from the respective
;;;    object in the site object. 
;;;
;;; ARGUMENTS
;;; The string to be parsed.
;;;
;;; EXAMPLE
#|
(with-colportage "Test [[file projects/img.jpg]] or [[asset logo.png]]")
;; =>
;; (LET* ((MD
;;          (PARSE-MARKDOWN
;;           "Test [[file projects/img.jpg]] or [[asset logo.png]]"))
;;        (RESULT MD))
;;   (SETF RESULT
;;         (CL-PPCRE:REGEX-REPLACE-ALL
;;       "\\[\\[file\\s(.*?)\\]\\]" RESULT
;;          #'(LAMBDA
;;                (TARGET START END MATCH-START
;;                 MATCH-END
;;                 &REST ARGS)
;;              (DECLARE (IGNORE START END ARGS))
;;              (LET* ((REGEX
;;                       "(?:\\[\\[file\\s)|(?:\\]\\])")
;;                     (MATCH
;;                         (SUBSEQ TARGET MATCH-START
;;                                 MATCH-END))
;;                     (UID
;;                       (CL-PPCRE:REGEX-REPLACE-ALL
;;                        REGEX MATCH "")))
;;                (INSERT-FILE-PATH UID)))))
;;   (SETF RESULT
;;         (CL-PPCRE:REGEX-REPLACE-ALL
;;       "\\[\\[asset\\s(.*?)\\]\\]" RESULT
;;          #'(LAMBDA
;;                (TARGET START END MATCH-START
;;                 MATCH-END
;;                 &REST ARGS)
;;              (DECLARE (IGNORE START END ARGS))
;;              (LET* ((REGEX
;;                       "(?:\\[\\[asset\\s)|(?:\\]\\])")
;;                     (MATCH
;;                         (SUBSEQ TARGET MATCH-START
;;                                 MATCH-END))
;;                     (UID
;;                       (CL-PPCRE:REGEX-REPLACE-ALL
;;                        REGEX MATCH "")))
;;                (INSERT-ASSET-PATH UID)))))
;;   RESULT)
|#
;;; SYNOPSIS
(defmacro with-colportage (string)
  ;;; ****
  `(let* ((md (parse-markdown ,string))
          (result md))
     ;; parse files
     (setf result
           (cl-ppcre::regex-replace-all
            "\\[\\[file\\s(.*?)\\]\\]"
            result
            #'(lambda (target start end match-start match-end &rest args)
                (declare (ignore start end args))
                (let* ((regex "(?:\\[\\[file\\s)\|(?:\\]\\])")
                       (match (subseq target match-start match-end))
                       (uid (cl-ppcre:regex-replace-all regex match "")))
                  (insert-file-path uid)))))
     ;; parse assets
     (setf result
           (cl-ppcre::regex-replace-all
            "\\[\\[asset\\s(.*?)\\]\\]"
            result
            #'(lambda (target start end match-start match-end &rest args)
                (declare (ignore start end args))
                (let* ((regex "(?:\\[\\[asset\\s)\|(?:\\]\\])")
                       (match (subseq target match-start match-end))
                       (uid (cl-ppcre:regex-replace-all regex match "")))
                  (insert-asset-path uid)))))
     ;; parse page-uuid
     (setf result
           (cl-ppcre::regex-replace-all
            "\\[\\[page-uuid\\s(.*?)\\]\\]"
            result
            #'(lambda (target start end match-start match-end &rest args)
                (declare (ignore start end args))
                (let* ((regex "(?:\\[\\[page-uuid\\s)\|(?:\\]\\])")
                       (match (subseq target match-start match-end))
                       (uuid (cl-ppcre:regex-replace-all regex match "")))
                  (insert-page-path-by-uuid uuid)))))
     result))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; EOF template.lisp
