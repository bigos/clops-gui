(declaim (optimize (speed 0) (safety 2) (debug 3)))

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
   (bottom-right)
   (width)
   (height)))

(defclass/std text-box (box)
  ((text)))

;;; === methods ================================================================
(defmethod add-child ((parent-box box) (child-box box))
  (setf
   (parent child-box) parent-box)
  (recalculate-absolute child-box)
  (pushnew child-box (children parent-box)))

(defmethod move ((box box) xd yd)
  (setf (~> box top-left x) (+ (~> box top-left x) xd)
        (~> box top-left y) (+ (~> box top-left y) yd))
  (recalculate-absolute box))

(defmethod recalculate-absolute-root ((box box))
  (when (and (typep (parent box) 'gui-window:lisp-window))
    ;; top left absolutes same as x y
    (setf (~> box top-left absolute-x) (~> box top-left x)
          (~> box top-left absolute-y) (~> box top-left y))
    ;; bottom right absolutes same as x+width y+height
    (let ((cax (~> box top-left x (+ _ (~> box width))))
          (cay (~> box top-left y (+ _ (~> box height)))))
      (setf (~> box bottom-right) (make-instance 'coordinates
                                                 :x cax
                                                 :absolute-x cax
                                                 :y cay
                                                 :absolute-y cay)))))

(defmethod recalculate-absolute ((box box))
  (if (typep (parent box) 'gui-window:lisp-window)
      (recalculate-absolute-root box)

      (step
       (let
           ((parent-top-left-absolute-x nil)
            (parent-top-left-absolute-y nil))

         (when (or (null (~> box parent top-left absolute-x))
                   (null (~> box parent top-left absolute-y)))
           (recalculate-absolute (parent box)))
         (setf parent-top-left-absolute-x (~> box parent top-left absolute-x)
               parent-top-left-absolute-y (~> box parent top-left absolute-y))

         (setf (~> box top-left absolute-x) (+ parent-top-left-absolute-x (~> box top-left x))
               (~> box top-left absolute-y) (+ parent-top-left-absolute-y (~> box top-left y)))

         (let ((bw (~> box width))
               (bh (~> box height)))
           (if (null (~> box bottom-right))
               (setf (~> box bottom-right) (make-instance 'coordinates
                                                          :x          (~> box top-left x          (+ _ bw))
                                                          :absolute-x (~> box top-left absolute-x (+ _ bw))
                                                          :y          (~> box top-left y          (+ _ bh))
                                                          :absolute-y (~> box top-left absolute-y (+ _ bh))))
               (setf (~> box bottom-right x)          (~> box top-left x          (+ _ bw))
                     (~> box bottom-right absolute-x) (~> box top-left absolute-x (+ _ bw))
                     (~> box bottom-right y)          (~> box top-left y          (+ _ bh))
                     (~> box bottom-right absolute-y) (~> box top-left absolute-y (+ _ bh)))))))))

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
      (and (<= (~> box top-left absolute-x) (car mouse-at) (~> box bottom-right absolute-x))
           (<= (~> box top-left absolute-y) (cdr mouse-at) (~> box bottom-right absolute-y))))))

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
