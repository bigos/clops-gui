(declaim (optimize (speed 0) (safety 3) (debug 3)))
;;; ============================ package gui-window ============================
(in-package #:gui-window)

(defun set-rgba (color)
  (gui-color:set-rgba color))

(defclass/std lisp-window ()
  ((gir-window  :type (or gir::object-instance keyword)
                :documentation "Either gir window or symbol used in test drawing")
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
(defmethod add-child ((lisp-window lisp-window) (box gui-box:box))
  (setf (gui-box:parent box) lisp-window)
  (setf (gethash (sxhash box) (all-widgets lisp-window)) box)
  (pushnew box (children lisp-window))
  (gui-box:recalculate-absolute box))

(defmethod most-current-widget ((lisp-window lisp-window))
  (loop
    for w being the hash-value in (gui-window:all-widgets lisp-window)
    for mos = (gui-box:mouse-over-score w)
    for current-widget = (cond ((and mos (null minmos))
                                w)
                               ((and mos minmos (< mos minmos))
                                w)
                               (T current-widget))
    for minmos =  (cond ((and mos (null minmos))
                         mos)
                        ((and mos minmos (< mos minmos))
                         mos)
                        (t minmos))
    finally ;; (warn "minmos is ~S ~S" minmos current-widget)
            (return current-widget)))
