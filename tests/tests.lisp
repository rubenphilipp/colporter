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
;;; $$ Last modified:  12:51:11 Tue Jul 25 2023 CEST
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
             (file (colporter::make-file testfile
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
         (file (colporter::make-file testfile)))
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
         (eq 'home (colporter::template page))
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
                  (test-pathname "content/projects/testb.jpg"))
                 (colporter::make-file
                  (test-pathname "content/test.jpg"))))
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
;;; EOF tests.lisp
