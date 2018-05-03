(in-package :cl-user)

(defpackage #:swank-c-p-c-test-fiveam
  (:documentation "Tests for swank-c-p-c.lisp.")
  (:use :cl)
  (:export #:suite)
  (:import-from :it.bese.fiveam
                #:test
                #:explain!
                #:run!
                #:def-suite
                #:in-suite
                #:is
                ))

(in-package #:swank-c-p-c-test-fiveam)

(def-suite suite :description "Tests for SWANK-C-P-C contrib.")
(in-suite suite)

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
         '(("abc-123::multiple-value-bind")
           "abc-123::multiple-value-bind")
         (swank:completions "abc-123::m-v-b" "COMMON-LISP-USER")))))


(test hello-world
  (with-new-package (pkg "ABC-123" :use '("COMMON-LISP"))
    (export (list (intern "HELLO-WORLD" pkg)) pkg)
    (is (equalp
         '(("abc-123:hello-world") "abc-123:hello-world")
         (swank:completions "abc-123:h" "COMMON-LISP-USER")))

    (is (equalp
         '(("hello-world") "hello-world")
         (swank:completions "h-world" "abc-123")))))

(test hello-world-ambiguous
  (with-new-package (pkg "ABC-123" :use '("COMMON-LISP"))
    (export (list (intern "HELLO-WORLD" pkg)) pkg)
    (export (list (intern "HELLO-WORLD-X" pkg)) pkg)
    (is (equalp
         '(("abc-123:hello-world" "abc-123:hello-world-x") "abc-123:hello-world")
         (swank:completions "abc-123:h" "COMMON-LISP-USER")))

    (is (equalp
         '(("hello-world" "hello-world-x") "hello-world")
         (swank:completions "h-world" "abc-123")))))

;;;; Test swank::longest-compound-prefix, even though it is internal.
;;;
;;; TODO: Many of the current behaviors seem... less than ideal.
;;; ("ab-efg:x1-yy" "abc-efg:x2-yy") should probably result in a prefix
;;; of "ab-efg:x-yy."

(test longest-compound-prefix-1
  (is (equalp (swank::longest-compound-prefix '("x1" "x2"))
              "x")))

(test longest-compound-prefix-2
  (is (equalp "x"
              (swank::longest-compound-prefix '("x1-yy" "x2-yy")))))

(test longest-compound-prefix-3
  (is (equalp "abc:x"
              (swank::longest-compound-prefix '("abc:x1-yy" "abc:x2-yy")))))

(test longest-compound-prefix-4
  (is (equalp "ab"
              (swank::longest-compound-prefix '("ab:x1-yy" "abc:x2-yy")))))

(test longest-compound-prefix-5
  (is (equalp "ab"
              (swank::longest-compound-prefix
               '("ab-efg:x1-yy" "abc-efg:x2-yy")))))

(test longest-compound-prefix-6
  (is (equalp "ab"
              (swank::longest-compound-prefix
               '("ab:x1-yy" "abc-efg:x2-yy")))))

(test longest-compound-prefix-7
  (is (equalp "ab"
              (swank::longest-compound-prefix
               '("ab::x1-yy" "abc-efg:x2-yy")))))

(test longest-compound-prefix-8
  (is (equalp "foo-bar:"
              (swank::longest-compound-prefix
               '("foo-bar::a-b-c" "foo-bar:a-b-c")))))
