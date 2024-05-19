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
