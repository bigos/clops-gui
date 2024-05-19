(in-package #:gui-box)

;;; === classes ================================================================

(defclass/std coordinates ()
  ((x)
   (y)))

(defclass/std box ()
  ((parent)
   (children)
   (top-left)
   (width)
   (height)))

(defclass/std text-box (box)
  ((text)))

;;; === methods ================================================================
(defmethod add-child ((parent-box box) (child-box box))
  (setf (parent child-box) parent-box)
  (pushnew child-box (children parent-box)))


(defmethod root-window ((box box))
  (if (typep (parent box) 'gui-window:lisp-window)
      (parent box)
      (root-window box)))

(defmethod mouse-overp ((box box))
  (when (equal (~> gui-window:*lisp-app* gui-window:current-motion)
               (root-window box))
    (let ((mouse-at (~> gui-window:*lisp-app* gui-window:mouse-coordinates)))
      (warn "current coordinates ~S ~S"
            (and (<= (x (top-left box)) (car mouse-at) (+ (x (top-left box)) (width  box)) )
                 (<= (y (top-left box)) (cdr mouse-at) (+ (y (top-left box)) (height box))))
            mouse-at))))
