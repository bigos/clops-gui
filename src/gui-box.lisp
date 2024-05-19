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
