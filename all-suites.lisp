(cl:defpackage #:swank-test
  (:export #:run-tests)
  (:use :cl))

(in-package #:swank-test)

(defparameter *suites* '(swank-c-p-c-test-fiveam:suite)
  "Names of FiveAM test suites to run.")

(defun run-tests ()
  "Returns T if all tests passed."
  (every 'fiveam:run! *suites*))
