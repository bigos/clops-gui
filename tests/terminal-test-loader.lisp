;; sbcl --noinform --load ./terminal-test-loader.lisp

(format t "~&running terminal test loader~%~%")

(ql:quickload :clops-gui/tests)
(asdf:test-system :clops-gui/tests)

(sb-ext:quit)
