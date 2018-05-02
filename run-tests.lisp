;;;; This file is meant to be loaded from the command line:
;;;;    sbcl --noinform --load "run-tests.lisp"
;;;;
;;;; To load the tests interactively, try
;;;;     (load "swank.asd")
;;;;     (asdf:load-system :swank)
;;;;     (swank-loader:init :load-contribs t)
;;;;     (let ((prove:*debug-on-error* t))
;;;;

;;;;
;;;; debugger on test failure.

(cl:defpackage #:swank-run-tests
  (:use :cl)
  (:export #:run-tests-and-quit
           #:run-tests-interactively
           #:main))

(cl:in-package :swank-run-tests)

(require 'asdf)
(require 'uiop)

(load (merge-pathnames "swank.asd" *load-pathname*))
(asdf:load-system :swank)
(swank-loader:init :load-contribs t)
(asdf:load-system :swank/test)

(defun run-tests-and-quit ()
  (let* ((prove:*enable-colors* t)
         (passed? (prove:run :swank/test :reporter :tap)))
    (uiop:quit (if passed? 0 1))))

(defun run-tests-interactively ()
  (let* ((prove:*enable-colors* t)
         (prove:*debug-on-error* t))
    (prove:run :swank/test :reporter :tap)))

(defun main (&optional (argv uiop:*command-line-arguments*))
  (if (member "--then-quit" argv :test #'string=)
      (run-tests-and-quit)
      (run-tests-interactively)))

(main)
