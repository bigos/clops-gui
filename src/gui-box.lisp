(in-package #:gui-box)

;;; === classes ================================================================

(defclass/std coordinates ()
  ((x)
   (y)
   (absolute-x)
   (absolute-y)))

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
  (setf (parent child-box) parent-box) ; that may interfere with the envisioned relative coordinates
  (pushnew child-box (children parent-box)))

(defmethod root-window ((box box))
  (if (typep (parent box) 'gui-window:lisp-window)
      (parent box)
      (when box
        (root-window box))))

(defmethod parent-boxes ((box box))
  (loop for po = box then (gui-box:parent po)
        until (typep po 'gui-window:lisp-window)
        collect po))

(defmethod mouse-overp ((box box))
  (when (equal (~> gui-window:*lisp-app* gui-window:current-motion)
               (root-window box))
    (let ((mouse-at (~> gui-window:*lisp-app* gui-window:mouse-coordinates)))
      (and (<= (x (top-left box)) (car mouse-at) (+ (x (top-left box)) (width  box)))
           (<= (y (top-left box)) (cdr mouse-at) (+ (y (top-left box)) (height box)))))))

(defmethod box-mouse ((box box))
  (if (mouse-overp box)
      (if (zerop (~> gui-window:*lisp-app* gui-window:mouse-button))
          :mouse-over
          :mouse-pressed)
      :mouse-out))

(defmethod box-color ((box box))
  (case (box-mouse box)
    (:mouse-out "lime")
    (:mouse-over "yellow")
    (:mouse-pressed "red")))
