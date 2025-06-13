;;;
(format t "~&running terminal test loader~%")

(ql:quickload :clops-gui/tests)
(asdf:test-system :clops-gui/tests)

(format t "~&Press Ctrl-d to exit ~%")
