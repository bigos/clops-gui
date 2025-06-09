(in-package #:clops-gui-tests)

(def-suite clops-gui-suite
  :description "Suite of other suites")
(in-suite clops-gui-suite)

(test test-equality
      "test some very basic equalities"
      (is (= 2 2))
      (is (= 4 (* 2 2)))
      ;; a little example for checking error hierarchy
      (signals error (/ 1 0))
      (signals arithmetic-error (/ 2 0))
      (signals division-by-zero (/ 3 0)))
