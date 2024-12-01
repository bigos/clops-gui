(declaim (optimize (speed 0) (safety 3) (debug 3)))

#|
loading
|#

;; (load "~/Programming/Lisp/clops-gui/examples/counter-third.lisp")
(push #p "~/Programming/Lisp/clops-gui/" ql:*local-project-directories*)
(ql:quickload '(:clops-gui :access :fiveam) :silent nil)


#|

** planning

*** event is processed, either Gtk4 or simulated event
that is decided on the type of the window attribute used in the event
file:~/Programming/Lisp/clops-gui/examples/counter-second.lisp::157

*** redraw-canvas is called
file:~/Programming/Lisp/clops-gui/src/gui-window.lisp::17
depending if real gtk4 or simulated event based on window properties
we either
**** enqueue gtk4 drawing which calls back to draw-func
file:~/Programming/Lisp/clops-gui/src/gui-drawing.lisp::12

**** or simulate drawing
file:~/Programming/Lisp/clops-gui/src/gui-drawing.lisp::34

*** funcall draw-window
file:~/Programming/Lisp/clops-gui/examples/counter-second.lisp::114

*** wait for the next event

|#
