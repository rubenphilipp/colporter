;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****c* named-object/colporter
;;; NAME
;;; colporter
;;; 
;;; AUTHOR
;;; Ruben Philipp <me@rubenphilipp.com>
;;;
;;; CREATED
;;; 2023-07-24
;;; 
;;; PURPOSE
;;; Implementation of the colporter class and related methods and functions.
;;; This class is the main class used for "assembling" websites.
;;;
;;; A colporter object holds gathers a site object and the data related to
;;; the output options of the project (e.g. output location, output location
;;; for asset files, a default error page etc.).
;;; Its output methods eventually build the page and create/move the respective
;;; files to the desired output location, which could then be used as a source
;;; for publishing the site.
;;;
;;; CLASS HIERARCHY
;;; named-object -> colporter
;;;
;;; $$ Last modified:  18:32:31 Tue Jul 25 2023 CEST
;;; ****
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :colporter)

(defclass colporter (named-object)
  ;; a short (optional) description of the colporter object
  ((description :accessor description :initarg :description :initform "")
   ;; a site object
   (site :accessor site :initarg :site :initform nil)
   ;; a page-id relating to a page object in the pages slot of the site object
   ;; which will be used as the default (404-)error page
   (error-page :accessor error-page :initarg :error-page :initform nil)
   ;; the output (base) directory of the colporter site
   (output-dir :accessor output-dir :initarg :output-dir :initform nil)
   ;; the suffix for html pages generated via colporter (e.g. "html")
   (output-suffix :accessor output-suffix :initarg :output-suffix :initform nil)
   ;; the default template to be used when no template is given or the desired
   ;; template is not available in the site (i.e. a key to one template in the
   ;; templates hash-table in the site object)
   (default-template :accessor default-template :initarg :default-template
                     :initform nil)))


(defmethod initialize-instance :after ((cp colporter) &rest initargs)
  (declare (ignore initargs))
  (update cp))

(defmethod print-object :before ((cp colporter) stream)
  (format stream "~%COLPORTER: description: ~a, ~
                  output-dir: ~a, asset-base-dir (rel. to output-dir): ~a, ~
                  output-suffix: ~a, default-template: ~a, error-page: ~a"
          (description cp) (output-dir cp) (asset-base-dir cp)
          (output-suffix cp) (default-template cp) (error-page cp)))

(defmethod (setf output-dir) :after (value (cp colporter))
  (declare (ignore value))
  (update cp))

(defmethod (setf asset-base-dir) :after (value (cp colporter))
  (declare (ignore value))
  (update cp))

(defmethod (setf error-page) :after (value (cp colporter))
  (declare (ignore value))
  (update cp))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; update all slots and check consistency
;;; 2023-07-25

(defmethod update ((cp colporter))
  ;; sanity checks
  (unless (typep (site cp) 'site)
    (error "colporter::update: The slot value of site is not if type SITE, ~
            but ~a" (type-of (site cp))))
  ;; test if error-page is available in the site object
  (unless (get-page (site cp) (error-page cp))
    (error "colporter::update: The error-page ~a does not exist in the ~
            site object. " (error-page cp)))
  ;; ensure trailing slash of output-dir
  (setf (slot-value cp 'output-dir) (trailing-slash (output-dir cp)))
  ;; ensure output suffix is a string
  (unless (stringp (output-suffix cp))
    (error "colporter::update: The output-suffix must be of type STRING, ~
            not ~a" (type-of (output-suffix cp))))
  ;; test if the default template exists in the site object
  (unless (get-template (site cp) (default-template cp))
    (error "colporter::update: The default-template ~a does not exist in ~
            the site object. " (default-template cp)))
  ;; set output-dir as data
  (setf (slot-value cp 'data) (output-dir cp))
  cp)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****f* colporter/make-colporter
;;; AUTHOR
;;; Ruben Philipp <me@rubenphilipp.com>
;;;
;;; CREATED
;;; 2023-07-24
;;; 
;;; DESCRIPTION
;;; A helper function to make a colporter object.
;;; NB: Making an instance of a colporter object does not imply the generation
;;; of a site but just assembling the respective data. 
;;;
;;; ARGUMENTS
;;; - A site object.
;;; 
;;; OPTIONAL ARGUMENTS
;;; keyword-arguments:
;;; - :output-dir. The output directory. This is a string which is the output
;;;   directory / base path of the site to be generated via related methods.
;;;   Default = The :output-dir as set in +clptr-config-data+.
;;; - :error-page. This is a key to an element in the hash-table
;;;   contained in the pages slot of the site object which will be used as the
;;;   standard error-page (e.g. for 404s). Default = "error"
;;; - :output-suffix. The suffix for html files generated from page objects.
;;;   Default = The :html-out-suffix as set in +clptr-config-data+.
;;; - :default-template. The default template to be used when no template is
;;;   given or the desired template (i.e. a key to one template in the
;;;   templates hash-table in the site object). Default = The :default-template
;;;   according to +clptr-config-data+.
;;; - :description. A short textual description of the object. Default = ""
;;; 
;;; RETURN VALUE
;;; A colporter object.
;;;
;;; SYNOPSIS
(defun make-colporter (site &key
                              (output-dir (get-clptr-config :output-dir))
                              (error-page "error")
                              (output-suffix (get-clptr-config :output-suffix))
                              (default-template
                               (get-clptr-config :default-template))
                              (description ""))
  ;;; ****
  (make-instance 'colporter :default-template default-template
                            :output-suffix output-suffix
                            :output-dir output-dir
                            :error-page error-page
                            :site site
                            :description description))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****m* colporter/build
;;; AUTHOR
;;; Ruben Philipp <me@rubenphilipp.com>
;;;
;;; CREATED
;;; 2023-07-24
;;; 
;;; DESCRIPTION
;;; This method builds the html site based on a colporter object.
;;; It generates and moves all files to the respective output locations. 
;;;
;;; ARGUMENTS
;;; A colporter-object. 
;;; 
;;; OPTIONAL ARGUMENTS
;;; keyword-arguments:
;;; - :verbose. Print occasional status messages to the stream. Default = t.
;;; 
;;; RETURN VALUE
;;; The output-dir. 
;;;
;;; SYNOPSIS
(defmethod build ((clptr colporter)
                  &key
                    (verbose t))
  ;;; ****
  (when verbose
    (format t "~%**********~% ~
                 COLPORT BUILD STARTED ~%~%"))
  ;; test if output directory exists
  (ensure-directories-exist output-dir :verbose verbose)
  ;; move all assets to the asset-base-dir in the output-dir
  (when verbose
    (format t "- moving assets ~%"))
  (let* (;; the absolute path to the asset-base-dir
         (assets-base (concatenate 'string
                                   (output-dir clptr)
                                   (asset-base-dir
                                    (site clptr)))))
    (print assets-base)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; EOF colporter.lisp
