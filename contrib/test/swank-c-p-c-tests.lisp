(in-package :cl-user)

(defpackage #:swank-c-p-c-test
  (:use :cl)
  (:import-from :prove #:ok #:is #:isnt #:plan #:finalize))

(in-package #:swank-c-p-c-test)

(plan nil)

(is (swank::longest-compound-prefix2
     '("x1" "x2"))
    "x")


(is (swank::longest-compound-prefix2
     '("x1-yy" "x2-yy"))
    "x-yy")


(is (swank::longest-compound-prefix2
     '("abc:x1-yy" "abc:x2-yy"))
    "abc:x-yy")


(is (swank::longest-compound-prefix2
     '("ab:x1-yy" "abc:x2-yy"))
    "ab:x-yy")

(is (swank::longest-compound-prefix2
     '("ab-efg:x1-yy" "abc-efg:x2-yy"))
    "ab-efg:x-yy")

(is (swank::longest-compound-prefix2
     '("ab:x1-yy" "abc-efg:x2-yy"))
    "ab:x-yy")

(is (swank::longest-compound-prefix2
     '("ab::x1-yy" "abc-efg:x2-yy"))
    "ab:x-yy")

(is (swank::longest-compound-prefix2
     '("foo-bar::a-b-c" "foo-bar:a-b-c"))
    "foo-bar:a-b-c")

(finalize)
