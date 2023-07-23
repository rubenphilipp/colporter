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
;;; $$ Last modified:  14:43:45 Sat Jul 15 2023 CEST
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

(defun run-tests ()
  (run! 'colporter))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; TESTS:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; test trailing slash
;;; RP  Sat Jul 15 14:43:24 2023
(test trailing-slash
  (is (equal "/trailing/test/"
             (colporter::trailing-slash "/trailing/test"))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; EOF tests.lisp
