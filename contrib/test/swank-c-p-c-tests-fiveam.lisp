(in-package :cl-user)

(defpackage #:swank-c-p-c-test-fiveam
  (:use :cl)
  (:import-from :it.bese.fiveam
                #:test
                #:run
                #:explain!
                #:run!
                #:def-suite
                #:in-suite
                #:is
                ))

(in-package #:swank-c-p-c-test-fiveam)

(def-suite swank-c-p-c-suite :description "Tests for SWANK-C-P-C contrib.")
(in-suite swank-c-p-c-suite)

(defun call-with-new-package (fn name &key use nicknames)
  "Constructs a package by passing MAKE-PACKAGE-ARGS to MAKE-PACKAGE, calls FN
with the new package, then deletes the package."
  (let ((package (make-package name :use use :nicknames nicknames)))
    (unwind-protect (funcall fn package)
      (delete-package package))))

(defmacro with-new-package ((package-var name &rest args &key use nicknames) &body body)
  "Calls BODY with a new package bound the the variable PACKAGE-VAR.

MAKE-PACKAGE-ARGS are passed to MAKE-PACKAGE to construct the package."
  (declare (ignore use nicknames))
  `(call-with-new-package (lambda (,package-var) ,@body)
                          ,name ,@args))


;;;; Test swank:completions.
(test mvb
  (with-new-package (pkg "ABC-123" :use '("COMMON-LISP"))
    (is (equalp
         (swank:completions "abc-123::m-v-b" "COMMON-LISP-USER")
         '(("abc-123::multiple-value-bind")
           "abc-123::multiple-value-bind")))))

(test hello-world
  (with-new-package (pkg "ABC-123" :use '("COMMON-LISP"))
    (export (list (intern "HELLO-WORLD" pkg)) pkg)
    (is (equalp
         (swank:completions "abc-123:h" "COMMON-LISP-USER")
         '(("abc-123:hello-world") "abc-123:hello-world")))

    (is (equalp
         (swank:completions "h-world" "abc-123")
         '(("hello-world") "hello-world")))))

(test hello-world-ambiguous
  (with-new-package (pkg "ABC-123" :use '("COMMON-LISP"))
    (export (list (intern "HELLO-WORLD" pkg)) pkg)
    (export (list (intern "HELLO-WORLD-X" pkg)) pkg)
    (is (equalp
         (swank:completions "abc-123:h" "COMMON-LISP-USER")
         '(("abc-123:hello-world" "abc-123:hello-world-x") "abc-123:hello-world")))

    (is (equalp
         (swank:completions "h-world" "abc-123")
         '(("hello-world" "hello-world-x") "hello-world")))))
