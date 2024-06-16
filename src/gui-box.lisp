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
   (height)
   (mouse-score)))

(defclass/std text-box (box)
  ((text)
   (recalculate)))

;;; === methods ================================================================
(defmethod add-child ((parent-box box) (child-box box))
  (setf
   (parent child-box) parent-box
   (gethash (sxhash child-box) (gui-window:all-widgets(root-window child-box))) child-box)

  (recalculate-absolute child-box)
  (pushnew child-box (children parent-box)))

(defmethod move ((box box) xd yd)
  (setf (~> box top-left x) (+ (~> box top-left x) xd)
        (~> box top-left y) (+ (~> box top-left y) yd))
  (recalculate-absolute box))

(defmethod resize ((box box) nwidth nheight)
  (unless (and (eq (~> box width) nwidth)
               (eq (~> box height) nheight))
    (setf (~> box width)  (+ nwidth  4)
          (~> box height) (+ nheight 4))
    (recalculate-absolute box)))

(defmethod recalculate-absolute-root ((box box))
  (if (and (typep (parent box) 'gui-window:lisp-window))
      (progn
        ;; top left absolutes same as x y
        (setf (~> box top-left absolute-x) (~> box top-left x)
              (~> box top-left absolute-y) (~> box top-left y))
        ;; bottom right absolutes same as x+width y+height
        (let ((cax (~> box top-left x (+ _ (~> box width))))
              (cay (~> box top-left y (+ _ (~> box height)))))
          (setf (~> box bottom-right) (make-instance 'coordinates
                                                     :x          (~> box top-left x          (+ _ (~> box width)))
                                                     :absolute-x (~> box top-left absolute-x (+ _ (~> box width)))
                                                     :y          (~> box top-left y          (+ _ (~> box height)))
                                                     :absolute-y (~> box top-left absolute-y (+ _ (~> box height)))))))
      (error "It should not be invoked unless the parent is a lisp-window")))

(defmethod recalculate-absolute ((box box))
  ;;(warn "type of box ~S and parent ~S" (type-of box) (type-of (parent box)))
  (if (typep (parent box) 'gui-window:lisp-window)
      ;;(warn "going to recalculate root")
      (recalculate-absolute-root box)

      (progn
        ;; ensure parent top left present
        (when (or (null (~> box parent top-left absolute-x))
                  (null (~> box parent top-left absolute-y)))
          (recalculate-absolute (parent box)))

        (let ((parent-top-left-absolute-x (~> box parent top-left absolute-x))
              (parent-top-left-absolute-y (~> box parent top-left absolute-y)))

          ;; top left
          (setf (~> box top-left absolute-x) (+ parent-top-left-absolute-x (~> box top-left x))
                (~> box top-left absolute-y) (+ parent-top-left-absolute-y (~> box top-left y))))

        ;; bottom right
        (let ((bw (~> box width))
              (bh (~> box height)))
          (if (typep (~> box bottom-right) 'coordinates)
              (setf (~> box bottom-right x)          (~> box top-left x          (+ _ bw))
                    (~> box bottom-right absolute-x) (~> box top-left absolute-x (+ _ bw))
                    (~> box bottom-right y)          (~> box top-left y          (+ _ bh))
                    (~> box bottom-right absolute-y) (~> box top-left absolute-y (+ _ bh)))
              (setf (~> box bottom-right) (make-instance 'coordinates
                                                         :x          (~> box top-left x          (+ _ bw))
                                                         :absolute-x (~> box top-left absolute-x (+ _ bw))
                                                         :y          (~> box top-left y          (+ _ bh))
                                                         :absolute-y (~> box top-left absolute-y (+ _ bh)))))))))

(defmethod root-window ((box box))
  (if (typep (parent box) 'gui-window:lisp-window)
      (parent box)
      (root-window (parent box))))

(defmethod parent-boxes ((box box))
  (loop for po = box then (gui-box:parent po)
        until (typep po 'gui-window:lisp-window)
        collect po))

;;; mouse over has all positives
(defmethod mouse-over-score ((box box))
  (if (equal (~> gui-window:*lisp-app* gui-window:current-motion)
             (root-window box))
      (let ((mouse-at (~> gui-window:*lisp-app* gui-window:mouse-coordinates)))
        (let ((tlx (- (car mouse-at)                   (~> box top-left absolute-x)))
              (tly (- (cdr mouse-at)                   (~> box top-left absolute-y)))
              (brx (- (~> box bottom-right absolute-x) (car mouse-at)))
              (bry (- (~> box bottom-right absolute-y) (cdr mouse-at))))

          (if (every (lambda (x) (>= x 0)) (list tlx tly brx bry))
              (setf (mouse-score box) (+ tlx tly brx bry))
              (setf (mouse-score box) nil))))
      (setf (mouse-score box) nil)))

(defmethod mouse-overp ((box box))
  (when (equal (~> gui-window:*lisp-app* gui-window:current-motion)
               (root-window box))
    (let ((mouse-at (~> gui-window:*lisp-app* gui-window:mouse-coordinates)))
      (and (<= (~> box top-left absolute-x) (car mouse-at) (~> box bottom-right absolute-x))
           (<= (~> box top-left absolute-y) (cdr mouse-at) (~> box bottom-right absolute-y))))))

(defmethod absolute-coordinates ((box box))
  (list (~> box top-left absolute-x)
        (~> box top-left absolute-y)
        (~> box bottom-right absolute-x)
        (~> box bottom-right absolute-y)))

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
