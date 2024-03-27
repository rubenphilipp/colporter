;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; FILE
;;; site.lisp
;;;
;;; NAME
;;; site
;;;
;;; DESCRIPTION
;;; This file constructs the website, based/built upon colporter
;;; (http://github.com/rubenphilipp/colporter)
;;;
;;; AUTHOR
;;; Ruben Philipp <me@rubenphilipp.com>
;;;
;;; CREATED
;;; 2024-03-27
;;;
;;; $$ Last modified:  23:28:36 Wed Mar 27 2024 CET
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :colporter)

(let* ((deploy-dir (path-from-same-dir "site/"))
       (staging-dir
         "/path/to/staging/dir/")
       (page-suffix "yaml")
       (title "Template")
       (author "William Loman")
       (snippets +site-snippets+)
       (templates +site-templates+)
       (assets (make-assets-from-dir (concatenate
                                      'string
                                      (path-from-same-dir "assets/"))))
       (pages (make-pages-from-dir (path-from-same-dir "content/")
                                   :page-suffix page-suffix))
       (files (make-files-from-dir (path-from-same-dir "content")
                                   :page-suffix page-suffix))
       (site (make-site snippets assets templates pages files
                        :asset-base-dir "assets/"
                        :page-root "/"
                        :data `(("title" . ,title)
                                ("author" . ,author))))
       (colporter-deploy (make-colporter site
                                         :output-dir deploy-dir
                                         :error-page "error"
                                         :output-suffix "html"
                                         :default-template "default"))
       (colporter-staging (make-colporter site
                                          :output-dir staging-dir
                                          :error-page "error"
                                          :output-suffix "html"
                                          :default-template "default")))
  (when nil
    (build colporter-deploy :verbose t))
  (when t
    (build colporter-staging :verbose t)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; EOF site.lisp
