;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****h* colporter tests
;;; NAME
;;; colporter tests
;;; 
;;; AUTHOR
;;; Ruben Philipp <me@rubenphilipp.com>
;;;
;;; CREATED
;;; 2023-07-09
;;; 
;;; PURPOSE
;;; Regression test suite for colporter.
;;;
;;; $$ Last modified:  01:01:50 Sun Jul 30 2023 CEST
;;; ****
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defpackage :colporter.tests
  (:use :cl :colporter :fiveam)
  (:shadow :test)
  (:export :run-tests))


(in-package :colporter.tests)

(def-suite colporter)
(in-suite colporter)

(defmacro test (name &body body)
  `(5am:test ,name
             ,@body))

(defmacro test-pathname (path)
  `(namestring (asdf::SYSTEM-RELATIVE-PATHNAME :colporter
                                               (concatenate 'string
                                                            "tests/"
                                                            ,path))))

(defun run-tests ()
  (run! 'colporter))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; TESTS:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; test trailing slash
;;; RP  Sat Jul 15 14:43:24 2023
(test test-trailing-slash
  (is (equal "/trailing/test/"
             (colporter::trailing-slash "/trailing/test"))))


;;; test make-file
;;; RP  Mon Jul 24 15:18:15 2023
(test test-make-file
      (let* ((testfile (test-pathname "style.css"))
             (file (colporter::make-file testfile "style.css"
                                         :description "A test file")))
        (is (and
             (equal "style.css" (colporter::filename file))
             (equal "css" (colporter::extension file))
             (equal '("text" "css") (colporter::type file))))))


;;; test make-asset
;;; RP  Mon Jul 24 15:28:19 2023
(test test-make-asset
  (let* ((testfile (test-pathname "style.css"))
         (asset (colporter::make-asset testfile "css/main")))
    (is (and
         (equal '("text" "css") (colporter::type asset))
         (equal "css/main" (colporter::uid asset))))))


;;; test imagep
;;; RP  Mon Jul 24 15:44:03 2023
(test test-imagep
  (let* ((testfile (test-pathname "test.jpg"))
         (file (colporter::make-file testfile "test.jpg")))
    (is (colporter::imagep file))))

;;; test snippet
;;; RP  Mon Jul 24 15:48:23 2023
(test test-snippet
  (let* ((snippet-fun #'(lambda (x y) (+ x y)))
         (snippet (colporter::make-snippet snippet-fun
                                           :description "addition")))
    (is (= (colporter::do-snippet snippet 4 5) 9))))

;;; test define-snippet
;;; RP  Mon Jul 24 16:22:53 2023
(test test-define-snippet
  (let* ((snippet-fun
           (colporter::define-snippet (title text)
             (colporter::with-html-string
               (:h1 title)
               (:p text))))
         (snippet (colporter::make-snippet snippet-fun))
         (result (colporter::do-snippet snippet "titel" "inhalt")))
    (is (equal result
               "<h1>titel</h1>
<p>inhalt"
               ))))

;;; test uid-from-path
;;; RP  Mon Jul 24 17:37:57 2023
(test test-uid-from-path
  (let ((result
          (colporter::uid-from-path
           "/sites/rubenphilipp/content/projects/opus-1.yaml"
           "/sites/rubenphilipp/content/")))
    (is (equal result "projects/opus-1"))))


;;; test make-page
;;; RP  Mon Jul 24 18:19:12 2023
(test test-make-page
  (let ((page (colporter::make-page
               (test-pathname "content/home.yaml")
               (test-pathname "content/"))))
    (is (and
         (equal "Home" (colporter::get-data page "title"))
         (equal "home" (colporter::template page))
         (typep (colporter::data page) 'hash-table)
         (equal "home" (colporter::uid page))))))

;;; test make-site
;;; RP  Tue Jul 25 01:08:28 2023
(test test-make-site
  (let* ((snippets (list
                    (colporter::make-snippet #'(lambda (x y) (+ x y))
                                             :id "sn1"
                                             :description "addition")
                    (colporter::make-snippet #'(lambda (x) (print x))
                                             :id "sn2"
                                             :description "just print")))
         (assets (list
                  (colporter::make-asset
                   (test-pathname "assets/style.css")
                   "style.css")))
         (templates (list
                     (cons 'home
                           (colporter::make-template
                            (colporter::define-template
                              "home")))
                     (cons 'project
                           (colporter::make-template
                            (colporter::define-template
                              "project")))))
         (base (test-pathname "content/"))
         (pages (list
                 (colporter::make-page
                  (test-pathname "content/home.yaml")
                  base)
                 (colporter::make-page
                  (test-pathname "content/projects/opus-1.yaml")
                  base)))
         (files (list
                 (colporter::make-file
                  (test-pathname "content/projects/testb.jpg")
                  "projects/testb.jpg")
                 (colporter::make-file
                  (test-pathname "content/test.jpg")
                  "test.jpg")))
         (site (colporter::make-site snippets assets templates
                                     pages files :data '((title . "Test")))))
    ;; test adding a value
    (colporter::set-data site 'author "RP")
    (colporter::set-data site 'test 12)
    (colporter::remove-data site 'test)
    (is (and
         (equal '("sn2" "sn1")
                (colporter::get-snippets site :objects nil))
         (typep (first (colporter::get-snippets site :objects t))
                'colporter::snippet)
         (eq nil (colporter::get-data site 'test))
         (equal "Test" (colporter::get-data site 'title))
         (typep (colporter::get-snippet site "sn1") 'colporter::snippet)
         (equal "RP" (colporter::get-data site 'author))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; test insert-snippet and yield-asset
;;; RP  Tue Jul 25 13:20:46 2023
(test test-insert-snippet-and-yield-asset
  (let* ((snippets (list
                    (colporter::make-snippet #'(lambda (x y) (+ x y))
                                             :id "sn1"
                                             :description "addition")
                    (colporter::make-snippet #'(lambda (x) (print x))
                                             :id "sn2"
                                             :description "just print")))
         (assets (list
                  (colporter::make-asset
                   (test-pathname "assets/style.css")
                   "style.css")))
         (templates (list
                     (cons 'home
                           (colporter::make-template
                            (colporter::define-template
                              (colporter::insert-snippet "sn1" 1 2))))
                     (cons 'project-b
                           (colporter::make-template
                            (colporter::define-template
                               (colporter::insert-asset-path "style.css"))))
                     (cons 'project
                           (colporter::make-template
                            (colporter::define-template
                              (colporter::uid
                               (colporter::yield-asset "style.css")))))))
         (base (test-pathname "content/"))
         (pages (list
                 (colporter::make-page
                  (test-pathname "content/home.yaml")
                  base)
                 (colporter::make-page
                  (test-pathname "content/projects/opus-1.yaml")
                  base)))
         (files (list
                 (colporter::make-file
                  (test-pathname "content/projects/testb.jpg")
                  "projects/testb.jpg")
                 (colporter::make-file
                  (test-pathname "content/test.jpg")
                  "test.jpg")))
         (site (colporter::make-site snippets assets templates
                                     pages files :data '((title . "Test"))
                                     :asset-base-dir "assets/"))
         (result-a (colporter::do-template
                       (colporter::get-template site 'home)
                     (colporter::get-page site "home")
                     site))
         (result-b (colporter::do-template
                       (colporter::get-template site 'project)
                     (colporter::get-page site "projects/opus-1")
                     site))
         (result-c (colporter::do-template
                       (colporter::get-template site 'project-b)
                     (colporter::get-page site "projects/opus-1")
                     site)))
    (is (and
         (equal "assets/style.css" result-c)
         (equal "Home" (colporter::get-data
                        (colporter::get-page site "home") "title"))
         (eq result-a 3)
         (equal "style.css" result-b)))))

;;; test make-colporter
;;; RP  Tue Jul 25 15:56:27 2023
(test test-make-colporter
  (let* ((snippets (list
                    (colporter::make-snippet #'(lambda (x y) (+ x y))
                                             :id "sn1"
                                             :description "addition")
                    (colporter::make-snippet #'(lambda (x) (print x))
                                             :id "sn2"
                                             :description "just print")))
         (assets (list
                  (colporter::make-asset
                   (test-pathname "assets/style.css")
                   "style.css")))
         (templates (list
                     (cons 'home
                           (colporter::make-template
                            (colporter::define-template
                              "home")))
                     (cons "default"
                           (colporter::make-template
                            (colporter::define-template
                              "default")))
                     (cons 'project
                           (colporter::make-template
                            (colporter::define-template
                              "project")))))
         (base (test-pathname "content/"))
         (pages (list
                 (colporter::make-page
                  (test-pathname "content/home.yaml")
                  base)
                 (colporter::make-page
                  (test-pathname "content/error.yaml")
                  base)
                 (colporter::make-page
                  (test-pathname "content/projects/opus-1.yaml")
                  base)))
         (files (list
                 (colporter::make-file
                  (test-pathname "content/projects/testb.jpg")
                  "projects/testb.jpg")
                 (colporter::make-file
                  (test-pathname "content/test.jpg")
                  "test.jpg")))
         (site (colporter::make-site snippets assets templates
                                     pages files
                                     :description "clptr test"
                                     :data '((title . "Test"))
                                     :asset-base-dir "assets/"))
         (colporter (colporter::make-colporter
                     site
                     :output-dir "/tmp/test-site/"
                     :error-page "error"
                     :output-suffix "html")))
    (is (equal "/tmp/test-site/" (colporter::data colporter)))))


;;; test make-files-from-dir
;;; RP  Tue Jul 25 16:45:59 2023
(test test-make-files-from-dir
  (let* ((dir (test-pathname "content/"))
         (result (colporter::make-files-from-dir dir
                                                 :page-suffix "yaml")))
    (is (typep result 'hash-table))))

;;; test make-pages-from-dir
;;; RP  Tue Jul 25 16:54:32 2023
(test test-make-pages-from-dir
  (let* ((dir (test-pathname "content/"))
         (result (colporter::make-pages-from-dir dir
                                                 :page-suffix "yaml")))
    (is (typep result 'hash-table))))

;;; test make-assets-from-dir
;;; RP  Tue Jul 25 16:45:59 2023
(test test-make-assets-from-dir
  (let* ((dir (test-pathname "assets/"))
         (result (colporter::make-assets-from-dir dir)))
    (is (typep result 'hash-table))))


;;; test insert-file-path
(test test-insert-file-path
  (let* ((snippets (list
                    (colporter::make-snippet #'(lambda (x y) (+ x y))
                                             :id "sn1"
                                             :description "addition")
                    (colporter::make-snippet #'(lambda (x) (print x))
                                             :id "sn2"
                                             :description "just print")))
         (assets (list
                  (colporter::make-asset
                   (test-pathname "assets/style.css")
                   "style.css")))
         (templates (list
                     (cons 'project
                           (colporter::make-template
                            (colporter::define-template
                              (colporter::insert-file-path "testb.jpg"))))
                     (cons 'home
                           (colporter::make-template
                            (colporter::define-template
                               (colporter::insert-file-path
                                   "projects/testb.jpg" nil))))))
         (base (test-pathname "content/"))
         (pages (list
                 (colporter::make-page
                  (test-pathname "content/home.yaml")
                  base)
                 (colporter::make-page
                  (test-pathname "content/projects/opus-1.yaml")
                  base)))
         (files (list
                 (colporter::make-file
                  (test-pathname "content/projects/testb.jpg")
                  "projects/testb.jpg")
                 (colporter::make-file
                  (test-pathname "content/test.jpg")
                  "test.jpg")))
         (site (colporter::make-site snippets assets templates
                                     pages files :data '((title . "Test"))
                                     :asset-base-dir "assets/"))
         (result-a (colporter::do-template
                       (colporter::get-template site 'home)
                     (colporter::get-page site "home")
                     site))
         (result-b (colporter::do-template
                       (colporter::get-template site 'project)
                     (colporter::get-page site "projects/opus-1")
                     site)))
    (is (and
         (equal "projects/testb.jpg"
                result-a)
         (equal "projects/testb.jpg"
                result-b)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; test build-colporter
;;; RP  Tue Jul 25 18:32:44 2023
(test test-build-colporter
  (let ((testdir "/tmp/testsite/"))
    ;; delete testsite
    (cl-fad::delete-directory-and-files testdir :if-does-not-exist :ignore)
    ;; load snippets
    (load (test-pathname "snippets/all.lisp"))
    ;; load templates
    (load (test-pathname "templates/all.lisp"))
    (let* ((snippets +clptr-test-snippets+)
           (assets (colporter::make-assets-from-dir (test-pathname "assets/")))
           (templates +clptr-test-templates+)
           (pages (colporter::make-pages-from-dir (test-pathname "content/")
                                                  :page-suffix "yaml"))
           (files (colporter::make-files-from-dir (test-pathname "content/")
                                                  :page-suffix "yaml"))
           (site (colporter::make-site snippets assets templates
                                       pages files
                                       :asset-base-dir "assets/"
                                       :data '(("title" . "Test"))))
           (colporter (colporter::make-colporter
                       site
                       :output-dir testdir
                       :error-page "error"
                       :output-suffix "html"
                       :default-template "default")))
      (colporter::build colporter)
      (is (and
           (let ((page
                   (colporter::get-page-by-uuid
                    site
                    "778425df-7450-4a18-b58b-9448f1de9b7a")))
             (typep page 'colporter::page))
           (equal "778425df-7450-4a18-b58b-9448f1de9b7a"
                  (colporter::uuid (colporter::get-page site "home")))
           (typep colporter 'colporter::colporter)
           (probe-file (concatenate 'string
                                    testdir
                                    "home.html")))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; EOF tests.lisp
