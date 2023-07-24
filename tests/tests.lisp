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
;;; $$ Last modified:  15:44:56 Mon Jul 24 2023 CEST
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
        (is (equal '("text" "css") (colporter::type file)))))


;;; test make-asset
;;; RP  Mon Jul 24 15:28:19 2023
(test test-make-asset
  (let* ((testfile (test-pathname "style.css"))
         (asset (colporter::make-asset testfile "css/main.css")))
    (is (and
         (equal '("text" "css") (colporter::type asset))
         (equal "css/main.css" (colporter::destination asset))))))


;;; test imagep
;;; RP  Mon Jul 24 15:44:03 2023
(test test-imagep
  (let* ((testfile (test-pathname "test.jpg"))
         (file (colporter::make-file testfile)))
    (is (colporter::imagep file))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; EOF tests.lisp
