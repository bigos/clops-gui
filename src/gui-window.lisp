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

;;; ========================== windows =========================================

(defclass/std lisp-app ()
  ((gtk4-app :type gir::object-instance)
   (windows :std (make-hash-table))
   (current-motion)
   (current-focus) ; it is not reliable for the last created window
   (mouse-coordinates)
   (mouse-button :std 0)))

(defun make-lisp-app (&optional gtk4-app)
  (make-instance 'lisp-app :gtk4-app gtk4-app))

(defclass/std lisp-window ()
  ((gir-window  :type (or gir::object-instance keyword)
                :documentation "Either gir window or symbol used in test drawing")
   (dimensions  :documentation "Cons with width and height or resized window")
   (children   :documentation "List of widgets whose parent is the window")
   (all-widgets :std (make-hash-table):documentation "All generations of widgets that eventually lead to the window. May be useful in finding current widget")))

;;; ====== all windows =========================================================
(defun all-windows ()
    (windows *lisp-app*))

;;; ========================== window manipulation =============================
(defmethod current-motion-window ((lisp-app lisp-app) (window t))
  (let ((m (window-hkey (current-motion lisp-app)))
        (w (window-hkey window)))
    (eq m w)))

(defmethod (setf current-focus) :before ((window T) (lisp-app lisp-app))
  (warn "~&??????????????????? setting current focus to ~s ~s~%~%"
        (window-hkey window)
        (type-of window)))

(defmethod current-focus-window  ((lisp-app lisp-app) (window t))
  (let ((h (window-hkey (current-focus lisp-app)))
        (w (window-hkey window)))
    (eq h w)))

(defmethod redraw-canvas ((window lisp-window) &optional log)
  (etypecase (gir-window window)
    (keyword
     (gui-drawing:simulate-draw-func (window-get *lisp-app* window) log))
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

(defun window-assert (window)
      (assert (or (typep window 'gir::object-instance)
                  (typep window 'sb-sys:system-area-pointer)
                  (typep window 'symbol))))

(defmethod window-get ((app lisp-app) (window T))
  ;; (warn "existing windows ~S" (loop for k being the hash-key of (windows app) collect k ))
  (gethash (window-hkey window)
           (windows app)))

(defmethod window-resize (w h win)
  (setf (dimensions (window-get *lisp-app* win)) (cons w h)))

(defmethod window-remove ((app lisp-app) (window T))
    (window-assert window)
  (if (remhash (window-hkey window) (windows app))
      (warn "success, window removed ~S" window)
      (warn "strange, window not removed ~S" window)))

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
    (setf (mouse-coordinates *lisp-app*) (cons x y)
          (current-motion    *lisp-app*) window))

(defmethod mouse-motion-leave ()
  (setf (mouse-coordinates *lisp-app*) nil
        (current-motion    *lisp-app*) nil))

(defmethod mouse-button-pressed (button)
  (incf (mouse-button *lisp-app*))
  (warn "pressed mouse button"))

(defmethod mouse-button-released ()
  (setf (mouse-button *lisp-app*) 0))
