(in-package #:clops-gui-tests)

(def-suite example-counter-tests
  :description "Suite for examples counter")

(in-suite example-counter-tests)

(test test-equality-counter
  "test some very basic equalities"
  (is (= 2 2))
  (is (= 4 (* 2 2)))
  ;; a little example for checking error hierarchy
  (signals error (/ 1 0))
  (signals arithmetic-error (/ 2 0))
  (signals division-by-zero (/ 3 0)))
