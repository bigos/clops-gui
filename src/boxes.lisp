(declaim (optimize (speed 0) (safety 3) (debug 3)))

(in-package #:common-lisp)

(in-package #:boxes)

;;; --- classes ----------------------------------------------------------------
(with-base-defclass coordinates () ((x :type integer) (y :type integer))
  coordinates-absolute
  coordinates-relative)

(defclass/std boxes-window (gui-window:lisp-window)
  ((width)
   (height)))
;;; and more classes in a separate file

(defclass/std box       ()
  ((parent)
   (coordinates-relative :type coordinates-relative)
   (coordinates-absolute :type coordinates-absolute)
   (width)
   (height)
   (color)
   (children)))

(defclass/std node (box)
  ())

(defclass/std node-text (box)
  ((text)
   (wrap :std 'truncate :type symbol)))

(defclass/std node-character (box)
  ((bchar)
   (font-size :std 15)
   (font-color :std "black")
   (font-face :std "Ubuntu Mono")
   (font-slant :std :normal)
   (font-weight :std :normal)))

(defclass/std node-horizontal (node)
  ())

(defclass/std node-vertical (node)
  ())

(defclass/std node-right (node-horizontal)
  ())

(defclass/std node-left (node-horizontal)
  ())

(defclass/std node-down (node-vertical)
  ())

(defclass/std node-up (node-vertical)
  ())

;;; !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

(defclass/std model       ()
  ((bbx)
   (bby)
   (grw)
   (grh)
   (tbw)
   (mouse-position)))

;;; ========================== constructors ====================================
(defun make-model ()
  (make-instance 'model
                 :bbx 20
                 :bby 20
                 :grw 30
                 :grh 30
                 :tbw 60
                 ))

(defun make-coordinates-absolute (x y)
  (make-instance 'coordinates-absolute :x x :y y))

(defun make-coordinates-relative (x y)
  (make-instance 'coordinates-relative :x x :y y))

(defun make-node (x y width height color)
  (make-instance 'node
                 :coordinates-relative (make-coordinates-relative x y)
                 :width  width
                 :height height
                 :color color))

(defun make-node-right (x y width height color)
  (make-instance 'node-right
                 :coordinates-relative (make-coordinates-relative x y)
                 :width  width
                 :height height
                 :color color))

(defun make-node-left (x y width height color)
  (make-instance 'node-left
                 :coordinates-relative (make-coordinates-relative x y)
                 :width  width
                 :height height
                 :color color))

(defun make-node-down (x y width height color)
  (make-instance 'node-down
                 :coordinates-relative (make-coordinates-relative x y)
                 :width  width
                 :height height
                 :color color))

(defun make-node-up (x y width height color)
  (make-instance 'node-up
                 :coordinates-relative (make-coordinates-relative x y)
                 :width  width
                 :height height
                 :color color))

(defun make-node-auto (width height color)
  (make-instance 'node
                 :width width
                 :height height
                 :color color))

;;; ========================== methods =========================================
(defmethod under-mouse-p ((node box))
  (let ((acx (x (coordinates-absolute node)))
        (acy (y (coordinates-absolute node)))
        (mpx (car (mouse-position *model*)))
        (mpy (cdr (mouse-position *model*))))
    (and
     acx
     acy
     mpx
     mpy
     (<= acx mpx (+ acx (width  node)))
     (<= acy mpy (+ acy (height node))))))

(defmethod mouse-over-p ((node box))
  (and (under-mouse-p node)
       (notany #'under-mouse-p (children node))))

(defmethod absolute-coordinates ((node box))
  (if (null (parent node))
      (setf (coordinates-absolute node) (make-coordinates-absolute
                                         (x (coordinates-relative node))
                                         (y (coordinates-relative node))))
      (setf (coordinates-absolute node) (make-coordinates-absolute
                                         (+ (x (coordinates-relative node))
                                            (x (coordinates-absolute (parent node))))
                                         (+ (y (coordinates-relative node))
                                            (y (coordinates-absolute (parent node))))))))

(defmethod absolute-coordinates :after ((node box))
  (loop for c in (children node)
        do (absolute-coordinates c)))

(defmethod render ((node box))
  ; (warn "parent type ~S widht ~S height ~S" (type-of (parent node)) (width node) (height node))
  (gui-color:set-rgba (color node))
  (cairo:rectangle   (x (coordinates-absolute node))
                     (y (coordinates-absolute node))
                     (width node)
                     (height node))
  (cairo:fill-path)
  ;; frame
  (when (mouse-over-p node)
    (render-frame node)))

(defmethod render :after ((node node))
  (loop for c in (children node)
        do (render c)))

(defun string-in-width (str box-width)
  (progn
    (cairo:select-font-face "Ubuntu Mono" :normal :normal)
    (cairo:set-font-size 20)
    (let ((my-text str))
      (loop for nwh = nil then wh
            for c across my-text
            for i = 1 then (1+ i)
            for str = (subseq my-text 0 i)
            for wh = (multiple-value-bind (xb yb width height)
                         (handler-bind
                             ((alexandria:simple-style-warning
                                (lambda (warning)
                                  (when (alexandria:starts-with-subseq
                                         "bare references to struct types are deprecated."
                                         (simple-condition-format-control warning))
                                    (muffle-warning warning)))))
                           (cairo:text-extents str))
                       (declare (ignore xb yb))
                       (cons width height))
            while (<= (car wh) (- box-width 16))
            finally (return (values str (car nwh) (cdr nwh)))))))

(defun render-node-text-truncate (node)
  (gui-color:set-rgba (color node))
  (cairo:rectangle   (x (coordinates-absolute node))
                     (y (coordinates-absolute node))
                     (width node)
                     (height node))
  (cairo:fill-path)

  (multiple-value-bind (str width height)
      (string-in-width (text node) (width node))
    (declare (ignore width))
    (when (null height)
      (setf height 20))
    (progn
      (cairo:move-to (x (coordinates-absolute node))
                     (+ height (y (coordinates-absolute node))))
      (gui-color:set-rgba "black")
      (cairo:show-text str))))

(defun render-node-text-wrap (node)
     (multiple-value-bind (str width height)
         (string-in-width (text node) (width node))
       (declare (ignore width))
       (when (null height)
         (setf height 20))
       (let ((text-fragments (loop
                               for tx = (text node) then (subseq tx (length str))
                               for txn = (subseq tx 0 (min  (length tx) (length str)))
                               collect txn
                               until (< (length txn) (length str))
                               )))

         (setf (height node) (* (+ height 2) (length text-fragments)))

         (gui-color:set-rgba (color node))
         (cairo:rectangle   (x (coordinates-absolute node))
                            (y (coordinates-absolute node))
                            (width node)
                            (height node))
         (cairo:fill-path)
         (loop for tf in text-fragments
               for line = 1 then (1+ line)
               do
                  (cairo:move-to (x (coordinates-absolute node))
                                 (+ (* line height)  (y (coordinates-absolute node))))
                  (gui-color:set-rgba "black")
                  (cairo:show-text tf)))))

(defmethod render ((node node-text))
  ;; text
  (cairo:select-font-face "Ubuntu Mono" :normal :normal)
  (cairo:set-font-size 20)

  (case (wrap node)
    (truncate
     (render-node-text-truncate node))
    (wrap
     (render-node-text-wrap node))
    (T (error "unhandled wrap ~s" (wrap node))))

  ;; frame
  (when (mouse-over-p node)
    (render-frame node)))

(defmethod render ((node node-character))
  (gui-color:set-rgba (color node))
  (cairo:rectangle   (x (coordinates-absolute node))
                     (y (coordinates-absolute node))
                     (width node)
                     (height node))
  (cairo:fill-path)

  (cairo:select-font-face (font-face node) (font-slant node) (font-weight node))
  (cairo:set-font-size (font-size node))
  (cairo:move-to (x (coordinates-absolute node))
                 (+ (y (coordinates-absolute node))
                    (1+ (floor (* (/ 3 4)  (font-size node))))))
  (gui-color:set-rgba (font-color node))
  (cairo:show-text (format nil "~A" (bchar node) ))
  ;; frame
  (when (mouse-over-p node)
    (render-frame node)))

(defmethod render-frame ((node box))
  (cairo:set-line-width 2.5)

  (gui-color:set-rgba "blue")
  (cairo:move-to
   (x (coordinates-absolute node))
   (y (coordinates-absolute node)))
  (cairo:line-to
   (+ (width node) (x (coordinates-absolute node)))
   (y (coordinates-absolute node)))
  (cairo:line-to
   (+ (width node)  (x (coordinates-absolute node)))
   (+ (height node) (y (coordinates-absolute node))))
  (cairo:stroke)

  (gui-color:set-rgba "red")
  (cairo:move-to
   (+ (width node)  (x (coordinates-absolute node)))
   (+ (height node) (y (coordinates-absolute node))))
  (cairo:line-to
   (x (coordinates-absolute node))
   (+ (height node) (y (coordinates-absolute node))))
  (cairo:line-to
   (x (coordinates-absolute node))
   (y (coordinates-absolute node)))
  (cairo:stroke))

;;; ============================================================================

(defmethod grow ((box box))
  ; do nothing
  )

(defmethod grow ((box node-right))
  (let ((wh (cons (loop for c in (children box) sum      (width  c))
                  (loop for c in (children box) maximize (height c)))))
    (setf (width  box) (car wh))
    (setf (height box) (cdr wh))))

(defmethod grow ((box node-left))
  (let ((wh (cons (loop for c in (children box) sum      (width  c))
                  (loop for c in (children box) maximize (height c)))))
    (setf (width  box) (car wh))
    (loop for c  in (reverse (children box))
          for sw = (width c) then (+ sw (width c))
          do (setf (coordinates-relative c)
                   (make-coordinates-relative (- sw
                                                 (width c))
                                              0)))
    (setf (height box) (cdr wh))))

(defmethod grow ((box node-down))
  (let ((wh (cons (loop for c in (children box) maximize (width  c))
                  (loop for c in (children box) sum      (height c)))))
    (setf (width  box) (car wh))
    (setf (height box) (cdr wh))))

(defmethod grow ((box node-up))
  (let ((wh (cons (loop for c in (children box) maximize (width  c))
                  (loop for c in (children box) sum      (height c)))))
    (setf (width  box) (car wh))
    (setf (height box) (cdr wh))
    (loop for c  in (reverse (children box))
          for sh = (height c) then (+ sh (height c))
          do (setf (coordinates-relative c)
                   (make-coordinates-relative 0
                                              (- sh
                                                 (height c)))))))

;;; ============================================================================
(defun %sum-children-dimension (parent dimension)
  (loop for c in (children parent)
        summing (funcall dimension c)))

(defmethod add-child :before ((parent box) (child box))
  (setf (parent child) parent))

(defmethod add-child ((parent box) (child box))
  (assert (slot-boundp child 'coordinates-relative) nil "coordinates-relative must be set"))

(defmethod add-child :after ((parent box) (child box))
  (setf (children parent) (append (children parent) (list child))))

(defmethod add-child ((parent node-right) (child box))
  (assert (not (slot-boundp child 'coordinates-relative)) nil "coordinates-relative must not be set")

  (setf (coordinates-relative child)
        (make-coordinates-relative (%sum-children-dimension parent 'width)
                                   0)))

(defmethod add-child ((parent node-left) (child box))
  (setf (coordinates-relative child)
        (make-coordinates-relative (- (width parent)
                                      (%sum-children-dimension parent 'width)
                                      (width child))
                                   0)))

(defmethod add-child ((parent node-down) (child box))
  (setf (coordinates-relative child)
        (make-coordinates-relative 0
                                  (%sum-children-dimension parent 'height))))

(defmethod add-child ((parent node-up) (child box))
  (setf (coordinates-relative child)
        (make-coordinates-relative 0
                                   (- (height parent)
                                      (%sum-children-dimension parent 'height)
                                      (height child)))))

;;; ============================================================================
(defmethod add-children ((parent box) children)
  (loop for cx in children
        do
           (if (and (atom cx) cx) ; leaf node
               (add-child parent cx)
               (loop for c in cx
                     do (add-child parent c))))
  (grow parent)
  parent)

(defmethod move-b1 ((model model) direction)
  (cond ((equal direction "Left")
         (setf (bbx model) (- (bbx model) 5)))
        ((equal direction "Right")
         (setf (bbx model) (+ (bbx model) 5)))

        ((equal direction "q")
         (setf (grw model) (- (grw model) 5)))
        ((equal direction "w")
         (setf (grw model) (+ (grw model) 5)))

        ((equal direction "b")
         (setf (grh model) (- (grh model) 5)))
        ((equal direction "h")
         (setf (grh model) (+ (grh model) 5)))

        ((equal direction "a")
         (setf (tbw model) (- (tbw model) 5)))
        ((equal direction "s")
         (setf (tbw model) (+ (tbw model) 5)))

        (T
         (warn "un-handled key"))))

;;; --- defparameter et al -----------------------------------------------------

(defparameter *model* nil)

;;; --- defgenerics (grouped by protocol) --------------------------------------

;;; --- inspectors -------------------------------------------------------------
(defun print-object-inner (obj  stream)
  (print-unreadable-object (obj stream :type t :identity t)
    (format stream "~a"
            (loop for sl in (sb-mop:class-slots (class-of obj))
                  for slot-name = (sb-mop:slot-definition-name sl)
                  collect (cons slot-name
                                (if (slot-boundp obj slot-name)
                                    (format nil "~S" (slot-value obj slot-name))
                                    (format nil "---unbound---" )))))))

;;; replace T with concrete classes
(defmethod print-object ((obj T) stream)
  (print-object-inner obj stream))

(defmethod print-object ((obj box) stream)
  (print-unreadable-object (obj stream :type t :identity t)
    (format stream "~a - ~S ~S"
            (color obj)
            (if (slot-boundp obj 'coordinates-absolute)
                (cons (x (coordinates-absolute obj))
                      (y (coordinates-absolute obj)))
                :unbound-absolute)
            (if (slot-boundp obj 'coordinates-relative)
                (cons (x (coordinates-relative obj))
                      (y (coordinates-relative obj)))
                :unbount-relative)
            )))

(defmethod print-object ((obj node-character) stream)
  (print-unreadable-object (obj stream :type t :identity t)
    (format stream "~S" (bchar obj))))

(defmethod print-object ((obj coordinates) stream)
  (print-object-inner obj stream))
