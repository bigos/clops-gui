(in-package #:gui-app)

(defparameter *lisp-app* nil)

;;; ========================== windows =========================================

(defclass/std lisp-app ()
  ((gtk4-app :type gir::object-instance)
   (windows :std (make-hash-table))
   (current-motion)
   (current-focus)    ; it is not reliable for the last created window
   (mouse-coordinates)
   (mouse-button :std 0)))

(defun make-lisp-app (&optional gtk4-app)
  (make-instance 'lisp-app :gtk4-app gtk4-app))

(defun all-windows ()
    (windows *lisp-app*))

(defmethod current-motion-window-p ((lisp-app lisp-app) (window t))
  (let ((m (gui-window:window-hkey (current-motion lisp-app)))
        (w (gui-window:window-hkey window)))
    (eq m w)))

(defmethod (setf current-focus) :before ((window T) (lisp-app lisp-app))
  (warn "~&??????????????????? setting current focus to ~s ~s~%~%"
        (gui-window:window-hkey window)
        (type-of window)))

(defmethod current-focus-window  ((lisp-app lisp-app) (window t))
  (let ((h (gui-window:window-hkey (current-focus lisp-app)))
        (w (gui-window:window-hkey window)))
    (eq h w)))

(defmethod window-get ((app lisp-app) (window T))
  ;; (warn "existing windows ~S" (loop for k being the hash-key of (windows app) collect k ))
  (gethash (gui-window:window-hkey window)
           (windows app)))

(defun window-assert (window)
  (assert (or (typep window 'gir::object-instance)
              (typep window 'sb-sys:system-area-pointer)
              (typep window 'symbol))))

(defmethod window-remove ((app lisp-app) (window T))
  (window-assert window)
  (if (remhash (gui-window:window-hkey window) (windows app))
      (warn "success, window removed ~S" window)
      (warn "strange, window not removed ~S" window)))

;;; ======================== window mouse handling =============================
(defmethod mouse-motion-enter ((window gui-window:lisp-window)  x y)
  (setf (gui-app:mouse-coordinates gui-app:*lisp-app*) (cons x y))
  (setf (gui-app:current-motion    gui-app:*lisp-app*) window))

(defmethod mouse-motion-leave ()
  (setf (gui-app:mouse-coordinates gui-app:*lisp-app*) nil)
  (setf (gui-app:current-motion   gui-app:*lisp-app*) nil))

(defmethod mouse-button-pressed (button)
  (incf (gui-app:mouse-button gui-app:*lisp-app*))
  (warn "pressed mouse button"))

(defmethod mouse-button-released ()
  (setf (gui-app:mouse-button gui-app:*lisp-app*) 0))
