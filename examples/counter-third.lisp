(declaim (optimize (speed 0) (safety 3) (debug 3)))

#|
loading
|#

;; (load "~/Programming/Lisp/clops-gui/examples/counter-third.lisp")
(push #p "~/Programming/Lisp/clops-gui/" ql:*local-project-directories*)
(ql:quickload '(:clops-gui :access :fiveam) :silent nil)


#| planning

  ** planning

  *** event is processed, either Gtk4 or simulated event
  that is decided on the type of the window attribute used in the event
  file:~/Programming/Lisp/clops-gui/examples/counter-second.lisp::157

  **** application state is updated

  **** relevant widgets are updated

  **** Layout is updated

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

;;; --------------------- package ----------------------------------------------
(defpackage #:counter-third
  (:use #:cl)
  (:import-from :serapeum
   :~>)
  (:import-from :defclass-std
   :defclass/std))

(in-package #:counter-third)

;;; -------------------------------- code --------------------------------------

;;; ---------------------------------- draw window -----------------------------
(defmethod draw-window ((window counter-third-window))
  ;; paint background
  (let ((cv 0.95)) (cairo:set-source-rgb  cv cv cv))
  (cairo:paint)

  (cairo:select-font-face "Ubuntu Mono" :italic :bold)
  (cairo:set-font-size 10)
  (cairo:move-to 10 10)
  (gui-window:set-rgba "black")
  (cairo:show-text (format nil "try moving the mouse over the window and outside of it"))

  (cairo:select-font-face "Ubuntu Mono" :italic :bold)
  (cairo:set-font-size 15)
  (cairo:move-to 10 100)
  (let ((cmotion (gui-app:current-motion-window-p gui-app:*lisp-app* window)))
    (if cmotion
        (gui-window:set-rgba "green")
        (gui-window:set-rgba "red"))
    (cairo:show-text (format nil "motion ~A" cmotion)))

  (cairo:set-font-size 30)
  (cairo:move-to 10 150)
  (gui-window:set-rgba "blue")
  (cairo:show-text (format nil "~A" (counted *model*)))

  (draw-rectangle *model* :a "yellow")
  (draw-rectangle *model* :b "orange"))

(defmethod draw-window :after ((window counter-third-window))
  ;; pink square follows the mouse
  (let ((app gui-app:*lisp-app*))
    (when (and (eq (gui-app:current-motion app)
                   window)
               (gui-app:mouse-coordinates app))
      (gui-window:set-rgba "pink")
      (cairo:rectangle
       (car (gui-app:mouse-coordinates app))
       (cdr (gui-app:mouse-coordinates app))
       25
       25)
      (cairo:fill-path))))

;;; -------------------------------- process event -----------------------------
(defmethod process-event ((lisp-window counter-third-window) event &rest args)
  (unless (member event '(:timeout :motion))
    (warn "event ~S ~S" event args))

  (case event
    (:timeout
     ;; do nothing yet
     )
    ((:motion :motion-enter)
     ;; we use simple case with one window so we ignore the window argument
     (destructuring-bind ((x y)) args
       ;; (update-mouse-location *model* x y)
       (gui-app:mouse-motion-enter lisp-window x y)))
    (:motion-leave
     (gui-app:mouse-motion-leave))
    (:focus-enter)
    (:focus-leave)
    (:pressed
     (destructuring-bind ((button x y)) args
       (declare (ignore button x y))
       ;; (update-mouse-press *model* x y button)
       ))
    (:released
     ;; (update-mouse-release *model*)
     (gui-app:mouse-button-released))
    (:scroll)
    (:resize
     (destructuring-bind ((w h)) args
       (gui-window:window-resize w h lisp-window)))
    (:key-pressed
     (destructuring-bind ((entered key-name key-code mods)) args
       (format t "~&>>> key pressed ~S~%" (list entered key-name key-code mods))
       (warn "model ~S" *model*)
       ))
    (otherwise
     (warn "not handled event ~S ~S" event args)))

  ;; moving widgets -------------------------
  ;; (warn "may implement moving widgets in response to actions)

  ;; redrawing ------------------------------
  (gui-window:redraw-canvas lisp-window (format  nil "~A" event)))

;;; ============================================================================
(defun main ()

  (setf
   gui-drawing:*client-fn-draw-objects*  'counter-third::draw-window
   gui-window-gtk:*client-fn-menu-bar*      nil
   gui-events:*client-fn-process-event* 'counter-third::process-event
   gui-window-gtk:*initial-window-width*    600
   gui-window-gtk:*initial-window-height*   400
   gui-window-gtk:*initial-title*           "Counter Third")

  (gui-window-gtk:window (make-instance 'counter-third-window)))
