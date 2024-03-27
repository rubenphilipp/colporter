;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; FILE
;;; site.lisp
;;;
;;; NAME
;;; Ruben Philipp's fictional website
;;;
;;; DESCRIPTION
;;; This file constructs the webseite of Ruben Philipp, based/built upon
;;; colporter (http://github.com/rubenphilipp/colporter
;;;
;;; AUTHOR
;;; Ruben Philipp <me@rubenphilipp.com>
;;;
;;; CREATED
;;; 2023-07-28
;;;
;;; $$ Last modified:  23:40:01 Wed Mar 27 2024 CET
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :colporter)

(let* ((output-dir (path-from-same-dir "site/"))
       (page-suffix "yaml")
       (title "Ruben Philipp")
       (author "Ruben Philipp")
       (snippets +rp-snippets+)
       (templates +rp-templates+)
       (assets (make-assets-from-dir (concatenate
                                      'string
                                      (path-from-same-dir "assets/"))))
       (pages (make-pages-from-dir (path-from-same-dir "content/")
                                   :page-suffix page-suffix))
       (files (make-files-from-dir (path-from-same-dir "content")
                                   :page-suffix page-suffix))
       (site (make-site snippets assets templates pages files
                        :asset-base-dir "assets/"
                        :data `(("title" . ,title)
                                ("author" . ,author))))
       (colporter (make-colporter site
                                  :output-dir output-dir
                                  :error-page "error"
                                  :output-suffix "html"
                                  :default-template "default")))
  (build colporter :verbose t))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; EOF site.lisp
