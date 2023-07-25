;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****h* clptr/markdown
;;; NAME
;;; markdown
;;; 
;;; AUTHOR
;;; Ruben Philipp <me@rubenphilipp.com>
;;;
;;; CREATED
;;; 2023-07-24
;;; 
;;; PURPOSE
;;; This module implements functionality for parsing markdown files. It is aimed
;;; at implementing referencing files and assets with markdown syntax.
;;;
;;; CLASS HIERARCHY
;;; none. no classes defined. 
;;;
;;; $$ Last modified:  20:22:43 Tue Jul 25 2023 CEST
;;; ****
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :colporter)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****f* markdown/parse-markdown
;;; AUTHOR
;;; Ruben Philipp <me@rubenphilipp.com>
;;;
;;; CREATED
;;; 2023-07-24
;;; 
;;; DESCRIPTION
;;; This functions parses a markdown string to HTML using cl-markdown
;;; (https://quickref.common-lisp.net/cl-markdown.html).
;;;
;;; This function is based on the markdown implementation in spinneret
;;; (https://github.com/ruricolist/spinneret).
;;;
;;; ARGUMENTS
;;; The string to parse containing markdown markup. 
;;; 
;;; RETURN VALUE
;;; The parsed string. 
;;;
;;; EXAMPLE
#|
(parse-markdown
 "# Test

  Lorem ipsum *dolor* [link](http://rubenphilipp.com).

  Fortsetzung...")
;; =>
;; "<h1>Test</h1><p>  Lorem ipsum <em>dolor</em>
;; <a href=\"http://rubenphilipp.com\">link</a>. </p><p>  Fortsetzung... </p>"
|#
;;; SYNOPSIS
(defun parse-markdown (string)
  ;;; ****
  (declare (string string))
  (let ((expansion
          (with-output-to-string (s)
            (let (markdown:*parse-active-functions*
                  markdown:*render-active-functions*)
              (markdown:markdown string
                                 :stream s
                                 :format :html)))))
    (if (search string expansion)
        string
        (if (find #\Newline string)
            expansion
            (spinneret::trim-ends "<p>" expansion "</p>")))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; EOF markdown.lisp
