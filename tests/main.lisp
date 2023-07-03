(defpackage barghest/tests/main
  (:use :cl
        :barghest
        :rove))
(in-package :barghest/tests/main)

;; NOTE: To run this test file, execute `(asdf:test-system :barghest)' in your Lisp.

(deftest test-target-1
  (testing "should (= 1 1) to be true"
  (format t "Testing~%")
    (ok (= 1 1))))