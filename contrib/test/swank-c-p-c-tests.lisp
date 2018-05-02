(in-package :cl-user)

(defpackage #:swank-c-p-c-test
  (:use :cl)
  (:import-from :prove
                #:deftest
                #:finalize
                #:is
                #:isnt
                #:plan
                #:ok
                ))

(in-package #:swank-c-p-c-test)

(plan nil)

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
(with-new-package (pkg "ABC-123" :use '("COMMON-LISP"))
  (is (swank:completions "abc-123::m-v-b" "COMMON-LISP-USER")
      '(("abc-123::multiple-value-bind")
        "abc-123n::multiple-value-bind"))

  (export (list (intern "HELLO-WORLD" pkg)) pkg)
  (is (swank:completions "abc-123:h" "COMMON-LISP-USER")
      '(("abc-123:hello-world") "abc-123:hello-world"))

  (is (swank:completions "h-world" "abc-123")
      '(("hello-world") "hello-world")))

(with-new-package (pkg "ABC-123" :use '("COMMON-LISP"))
  (export (list (intern "HELLO-WORLD" pkg)) pkg)
  (export (list (intern "HELLO-WORLD-X" pkg)) pkg)
  (is (swank:completions "abc-123:h" "COMMON-LISP-USER")
      '(("abc-123:hello-world" "abc-123:hello-world-x") "abc-123:hello-world"))

  (is (swank:completions "h-world" "abc-123")
      '(("hello-world" "hello-world-x") "hello-world")))


;;;; Test swank::longest-compound-prefix, even though the symbol is not
;;;; exported.


(is (swank::longest-compound-prefix
     '("x1" "x2"))
    "x")


(is (swank::longest-compound-prefix
     '("x1-yy" "x2-yy"))
    "x")


(is (swank::longest-compound-prefix
     '("abc:x1-yy" "abc:x2-yy"))
    "abc:x-yy")


(is (swank::longest-compound-prefix
     '("ab:x1-yy" "abc:x2-yy"))
    "ab:x-yy")

(is (swank::longest-compound-prefix
     '("ab-efg:x1-yy" "abc-efg:x2-yy"))
    "ab-efg:x-yy")

(is (swank::longest-compound-prefix
     '("ab:x1-yy" "abc-efg:x2-yy"))
    "ab:x-yy")

(is (swank::longest-compound-prefix
     '("ab::x1-yy" "abc-efg:x2-yy"))
    "ab:x-yy")

(is (swank::longest-compound-prefix
     '("foo-bar::a-b-c" "foo-bar:a-b-c"))
    "foo-bar:a-b-c")

(finalize)
