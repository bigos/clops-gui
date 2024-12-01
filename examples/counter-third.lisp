(declaim (optimize (speed 0) (safety 3) (debug 3)))

#|
loading
|#

;; (load "~/Programming/Lisp/clops-gui/examples/counter-third.lisp")
(push #p "~/Programming/Lisp/clops-gui/" ql:*local-project-directories*)
(ql:quickload '(:clops-gui :access :fiveam) :silent nil)
