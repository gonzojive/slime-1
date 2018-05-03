;;;; This file is meant to be loaded from the command line:
;;;;    sbcl --noinform --load "run-tests.lisp" --then-quit
;;;;
;;;; To load the tests interactively, try
;;;;     (asdf:operate 'asdf:test-op :swank/test)

(cl:defpackage #:swank-run-tests
  (:use :cl))

(cl:in-package :swank-run-tests)

(require "ASDF")
(load (merge-pathnames "swank.asd" *load-pathname*))
(asdf:load-system :swank/test)
(asdf:load-system :uiop)

(defun main (&optional (argv uiop:*command-line-arguments*))
  (let ((passed? (swank-test:run-tests)))
    (when (member "--then-quit" argv :test #'string=)
      (uiop:quit (if passed? 0 1)))
    passed?))

(main)
