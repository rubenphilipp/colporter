;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****h* colporter/globals
;;; NAME
;;; globals
;;; 
;;; AUTHOR
;;; Ruben Philipp <me@rubenphilipp.com>
;;;
;;; CREATED
;;; 2023-07-23
;;; 
;;; PURPOSE
;;; Definition of the configuration data and globals for colporter.
;;;
;;; CLASS HIERARCHY
;;; none. no classes defined.
;;;
;;; $$ Last modified:  14:15:02 Tue Jul 25 2023 CEST
;;; ****
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :colporter)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****v* globals/+clptr-config-data+
;;; NAME
;;; +kr-config-data+
;;; 
;;; AUTHOR
;;; Ruben Philipp <me@rubenphilipp.com>
;;;
;;; CREATED
;;; 2023-07-15
;;; 
;;; DESCRIPTION
;;; A global holding information about the configuration of colporter.
;;; 
(defparameter +clptr-config-data+
  `(;; the directory for temp files.
    (:temp-dir . "/tmp/")
    ;; the default template-key / -field in an YAML file
    (:template-key . "template")
    ;; the default output directory for colporter-generated sites
    (:output-dir . "/tmp/site/")
    ;; the standard default template hash key (cf. colporter class def.)
    (:default-template . "default")
    ;; the default html output suffix
    (:html-out-suffix . "html")
    ;; the default suffix to indicate files containing page data
    (:page-suffix . "yaml")))
  ;;; ****

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****f* globals/get-clptr-config
;;; AUTHOR
;;; Ruben Philipp <me@rubenphilipp.com>
;;;
;;; CREATED
;;; 2023-07-23
;;; 
;;; DESCRIPTION
;;; Returns the value of a configuration setting from the global
;;; +clptr-config-data+.
;;;
;;; SYNOPSIS
(defun get-clptr-config (key)
  ;;; ****
  (declare (special +clptr-config-data+))
  (assoc-value +clptr-config-data+ key))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****f* globals/set-clptr-config
;;; AUTHOR
;;; Ruben Philipp <me@rubenphilipp.com>
;;;
;;; CREATED
;;; 2023-07-23
;;; 
;;; DESCRIPTION
;;; Set the value of a element in the +clptr-config-data+ global.
;;;
;;; ARGUMENTS
;;; - The key to the element in +clptr-config-data+.
;;; - The new value.
;;; 
;;; RETURN VALUE
;;; The new value of the config element.
;;; 
;;; SYNOPSIS
(defun set-clptr-config (key value)
  ;;; ****
  (declare (special +clptr-config-data+))
  (setf (assoc-value +clptr-config-data+ key) value))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; EOF globals.lisp
