(declaim (optimize (speed 0) (safety 3) (debug 3)))
;;; ============================ package gui-window ============================
(in-package #:gui-window)

(defun set-rgba (color)
  (gui-color:set-rgba color))

(defclass/std lisp-window ()
  ((gir-window  :type (or gir::object-instance keyword)
                :documentation "Either gir window or symbol used in test drawing")
   (gir-menu-bar :type (or gir::object-instance null)
                 :documentation "Either gir menu or null")
   (gir-canvas :type (or gir::object-instance null)
               :documentation "Either gir canvas or null")
   (dimensions  :documentation "Cons with width and height or resized window")
   (children   :documentation "List of widgets whose parent is the window")
   (all-widgets :std (make-hash-table):documentation "All generations of widgets that eventually lead to the window. May be useful in finding current widget")))

;;; ========================== window manipulation =============================

(defmethod redraw-canvas ((window lisp-window) &optional log)
  (etypecase (gir-window window)
    (keyword
     (gui-drawing:simulate-draw-func (gui-app:window-get gui-app:*lisp-app* window) log))
    (t
     (gtk4:widget-queue-draw
      (serapeum:~> window gir-window gtk4:widget-first-child gtk4:widget-first-child)))))

(defmethod window-resize (w h win)
  (setf (dimensions (gui-app:window-get gui-app:*lisp-app* win)) (cons w h)))

(defmethod window-hkey ((window gir::object-instance))
  (cffi:pointer-address (gir::this-of window)))
(defmethod window-hkey ((window symbol))
  "This form is only used in simulated drawing, but not in gtk4"
  window)
(defmethod window-hkey ((window lisp-window))
  (window-hkey
   (gir-window window)))

;;; ============================ window child widgets ==========================
