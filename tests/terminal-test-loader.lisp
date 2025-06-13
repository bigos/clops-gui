;; sbcl --noinform --load ./terminal-test-loader.lisp

(format t "~&running terminal test loader~%~%")

(ql:quickload :clops-gui/tests)

(in-package #:clops-gui-tests)

(clops-gui-tests:run-all-tests)

(sb-ext:quit)
