;;; ============================ package gui-window ============================
(in-package #:gui-window)

(cffi:defcstruct gdk-rgba
    (red   :float)
  (green :float)
  (blue  :float)
  (alpha :float))

(defun color-to-rgba (color)
  (cffi:with-foreign-object (rgba '(:struct gdk-rgba))
    (let ((pointer (make-instance 'gir::struct-instance
                                  :class (gir:nget gdk::*ns* "RGBA")
                                  :this rgba)))
      (let ((valid-color (gdk:rgba-parse pointer color)))
        (cffi:with-foreign-slots ((red green blue alpha) rgba (:struct gdk-rgba))
          (list valid-color red green blue alpha))))))

(defun set-rgba (color)
  (let ((parsed-color (color-to-rgba color)))
    (if (first parsed-color)
        (apply 'cairo:set-source-rgba (rest parsed-color))
        (error "~S is not a valid color" color))))

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

(defmethod window-hkey ((window gir::object-instance))
  (cffi:pointer-address (gir::this-of window)))
(defmethod window-hkey ((window symbol))
  "This form is only used in simulated drawing, but not in gtk4"
  window)
(defmethod window-hkey ((window lisp-window))
  (window-hkey
   (gir-window window)))

(defmethod window-resize (w h win)
  (setf (dimensions (gui-app:window-get gui-app:*lisp-app* win)) (cons w h)))

;;; ============================ window child widgets ==========================
(defmethod add-child ((lisp-window lisp-window) (box gui-box:box))
  (setf (gui-box:parent box) lisp-window
        (gethash (sxhash box) (all-widgets lisp-window)) box)
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
    ;; do (if (gui-box:mouse-overp w)
    ;;        (format t "over ~S ~S ~S ~S~%" w mos (gui-box:mouse-score w) (gui-box:absolute-coordinates w))
    ;;        (format t "____ ~S ~S ~S ~S~%" w mos (gui-box:mouse-score w) (gui-box:absolute-coordinates w)))
    finally ;; (warn "minmos is ~S ~S" minmos current-widget)
            (return current-widget)))

;;; ======================== window mouse handling =============================
(defmethod mouse-motion-enter ((window lisp-window)  x y)
    (setf (gui-app:mouse-coordinates gui-app:*lisp-app*) (cons x y)
          (gui-app:current-motion    gui-app:*lisp-app*) window))

(defmethod mouse-motion-leave ()
  (setf (gui-app:mouse-coordinates gui-app:*lisp-app*) nil
        (gui-app:current-motion   gui-app:*lisp-app*) nil))

(defmethod mouse-button-pressed (button)
  (incf (gui-app:mouse-button gui-app:*lisp-app*))
  (warn "pressed mouse button"))

(defmethod mouse-button-released ()
  (setf (gui-app:mouse-button gui-app:*lisp-app*) 0))
