(defpackage chess/tests/main
  (:use :cl
        :chess
        :rove))
(in-package :chess/tests/main)

;; NOTE: To run this test file, execute `(asdf:test-system :chess)' in your Lisp.

(deftest test-target-1
  (testing "should (= 1 1) to be true"
    (ok (= 1 1))))
