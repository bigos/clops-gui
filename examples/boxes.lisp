(declaim (optimize (speed 0) (safety 3) (debug 3)))

;; (load "~/Programming/Lisp/clops-gui/examples/boxes.lisp")

(ql:quickload :clops-gui)

(in-package #:common-lisp)

(in-package #:boxes)

;;; --- classes ----------------------------------------------------------------
(with-base-defclass coordinates () ((x :type integer) (y :type integer))
  coordinates-absolute
  coordinates-relative)


(defclass/std boxes-window (window-canvas:lisp-window)
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
  (window-canvas:set-rgba (color node))
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
  (window-canvas:set-rgba (color node))
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
      (window-canvas:set-rgba "black")
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

         (window-canvas:set-rgba (color node))
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
                  (window-canvas:set-rgba "black")
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
  (window-canvas:set-rgba (color node))
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
  (window-canvas:set-rgba (font-color node))
  (cairo:show-text (format nil "~A" (bchar node) ))
  ;; frame
  (when (mouse-over-p node)
    (render-frame node)))

(defmethod render-frame ((node box))
  (cairo:set-line-width 2.5)

  (window-canvas:set-rgba "blue")
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

  (window-canvas:set-rgba "red")
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

(defmethod print-object ((obj coordinates) stream)
  (print-object-inner obj stream))

;;; === draw window ============================================================
(defun window-snap-shot (changeme)
  (warn "== screen shot")
  (setf window-canvas:*window-size-cons-code* (lambda () (cons 430 490)))
  (window-canvas::simulate-draw-func))

(defun show-my-text (x y font-size str &optional (color "black"))
  (window-canvas:set-rgba color)
  (cairo:select-font-face "Ubuntu Mono" :normal :normal)
  (cairo:set-font-size font-size)
  (cairo:move-to x y)
  (cairo:show-text str))

(defun text-size (text text-size)
  (cairo:select-font-face "Ubuntu Mono" :normal :normal)
  (cairo:set-font-size text-size)

  (multiple-value-bind (xb yb width height)
      (handler-bind
          ((alexandria:simple-style-warning
             (lambda (warning)
               (when (alexandria:starts-with-subseq
                      "bare references to struct types are deprecated."
                      (simple-condition-format-control warning))
                 (muffle-warning warning)))))

        (cairo:text-extents (format nil "~A" text)))
    (list :xb xb :yb yb :width width :height height)))

(defun add-children-text (world text-string)
  (let ((text-container (make-node 20
                                   340
                                   (- (width world) 20 20)
                                   (- (height world) 360) "yellow")))
    (add-children text-container
                  (let* ((fsize 18)
                         (margin-horizontal 0)
                         (margin-vertical 0)

                         (text-data (text-size "ly()" fsize ))
                         (twidth (floor (/ (getf text-data :width) 4)))
                         (theight (getf text-data :height))

                         (bwidth (+ twidth 0))
                         (bheight (+ theight 0)))
                    (loop for last-char = nil then c
                          for row = 0 then (if (equal last-char #\Newline) (1+ row) row)
                          for col = 0 then (if (equal last-char #\Newline)  0 (1+ col))
                          for relx = (+ margin-horizontal
                                        (ceiling
                                         (* col  (1+ twidth) )))
                          for rely = (+ margin-vertical
                                        (ceiling
                                         (* row (1+ theight))))
                          for c across text-string
                          unless (let ((max-x-coord (+ relx bwidth))
                                       (max-y-coord (+ rely bheight)))
                                   (or
                                    (>= max-x-coord (- (width text-container) 10))
                                    (>= max-y-coord (height text-container))))
                            collect (make-instance 'node-character
                                                   :bchar c
                                                   :font-size fsize
                                                   :coordinates-relative
                                                   (make-coordinates-relative
                                                    relx
                                                    rely)
                                                   :width bwidth
                                                   :height bheight
                                                   :color "pink"))))))

(defun draw-window (window-id)
  (assert (or (typep window-id 'integer)
              (typep window-id 'keyword)))
  (let ((window (window-canvas:get-window window-id)))
    (assert (typep window 'window-canvas:lisp-window))

    ;; paint background
    (let ((cv 0.9))
      (cairo:set-source-rgb  cv cv cv)
      (cairo:paint))

    ;; more to do
    (show-my-text 10 30 15 (format nil "Please code something"))

    ;; and more - dom structure

    ;; and more - drawing it
    (let ((world (make-node 0 0 (width window) (height window) "#cccccc88")))
      (absolute-coordinates world)

      (add-children world
                    (list
                     (add-children (make-node (bbx *model*)
                                              (bby *model*)
                                              50 50 "purple")
                                   (list
                                    (make-node 10 10 25 25 "violet")))))
      ;; --------------
      ;; https://www.w3.org/TR/SVG11/types.html#ColorKeywords
      ;;(warn "------------------------ adding tree nodes ------------------------------")
      (add-children world
                    (list
                     (add-children (make-node-right 20 80 100 100 "black")
                                   (list
                                    (make-node-auto 20 30 "red")
                                    (make-node-auto (grw *model*) (grh *model*) "green")
                                    (make-node-auto 40 30 "blue")))
                     (add-children (make-node-left 220 80 100 100 "black")
                                   (list
                                    (make-node-auto 20 30 "red")
                                    (make-node-auto (grw *model*) (grh *model*) "green")
                                    (make-node-auto 40 30 "blue")))

                     (add-children (make-node-down 20 220 101 101 "grey")
                                   (list
                                    (make-node-auto 30 20 "red")
                                    (make-node-auto (grw *model*) (grh *model*) "green")
                                    (make-node-auto 30 40 "blue")))
                     (add-children (make-node-up 220 220 101 101 "grey")
                                   (list
                                    (make-node-auto 30 20 "red")
                                    (make-node-auto (grw *model*) (grh *model*) "green")
                                    (make-node-auto 30 40 "blue")))

                     (add-children (make-node (floor (/ (width world) 2))
                                              50
                                              (- (floor (/ (width world) 2)) 20)
                                              (+ 0 280) "mediumseagreen")
                                   (list
                                    (make-instance 'node-text
                                                   :coordinates-relative (make-coordinates-relative 10 10)
                                                   :width (tbw *model*)
                                                   :height 30
                                                   :color "yellow"
                                                   :wrap 'truncate
                                                   :text "Trimming the first text was easy."
                                                   )
                                    (make-instance 'node-text
                                                   :coordinates-relative (make-coordinates-relative 10 50)
                                                   :width (tbw *model*)
                                                   :height 30
                                                   :color "white"
                                                   :wrap 'wrap
                                                   :text "Wrapping the second text is more challenging."
                                                   )
                                    ))
                     (add-children-text world
                                        (alexandria:read-file-into-string
                                         (let ((text-option 1))
                                           (case text-option
                                             (0 "~/Documents/Roams/README.org")
                                             (1 "~/Programming/Lisp/lispy-experiments/window-canvas-boxes/window-canvas-boxes.asd")
                                             (T "~/.bashrc"))))
                                        )))
      ;; === starting from world set absolute coordinates ===
      ;; (warn "adding absolute coordinates -----------------------------------")
      (absolute-coordinates world)

      ;; (warn "rendering -----------------------------------------------")
      (render world))))

;;; === events =================================================================
(defmethod process-gtk-event ((lisp-window boxes-window) event &rest args)
  (unless (member event '(:timeout :motion))
    (warn "event ~S ~S" event args))

  (case event
    (:timeout
          ;; do nothing yet
     )
    ((:motion :motion-enter)
     ;; we use simple case with one window so we ignore the window argument
     (destructuring-bind ((x y)) args
       (setf (mouse-position *model*) (cons x y))))
    (:motion-leave
     (setf (mouse-position *model*) nil))
    (:focus-enter)
    (:focus-leave)
    (:pressed
          (destructuring-bind ((button x y)) args
            (warn "pressed button ~A at ~a ~a" button x y)
            ;; (mouse-press *game* button x y)
            )
     )
    (:released
          (destructuring-bind ((button x y)) args
            (warn "released button ~A at ~a ~a" button x y)
            ;; (mouse-release *game* button x y)
            ))
    (:scroll)
    (:resize
          (destructuring-bind ((width height)) args
            (warn "resized with ~s ~s" width height)
            (setf window-canvas:*window-size-cons-code* (lambda () (cons
                                                                    width
                                                                    height)))
            (setf (width  lisp-window) width
                  (height lisp-window) height)))
    (:key-pressed
     (destructuring-bind ((entered key-name key-code mods)) args
       (warn "key pressed ~S~%" (list entered key-name key-code mods))
       (when (equal key-name "Pause")
         (window-snap-shot nil))
       ;; (game-key-pressed *game* entered key-name)
       (move-b1 *model* key-name)
       ))
    (otherwise
     (warn "not handled event ~S ~S" event args)))

  (window-canvas:redraw-canvas lisp-window (format  nil "~A" event)))

;;; === main ===================================================================
(defun init ()
  (progn
    (setf window-canvas:*gtk-client-fn-draw-objects*  'boxes::draw-window)
    (setf window-canvas:*gtk-client-fn-menu-bar*      nil)
    (setf window-canvas:*gtk-client-fn-process-event* 'boxes::process-gtk-event)
    (setf window-canvas:*gtk-initial-window-width*    790)
    (setf window-canvas:*gtk-initial-window-height*   750)
    (setf window-canvas:*gtk-initial-title*           "Boxes")
    (setf *model* (make-model))

    ))

(defun main ()
  (format T "boxes main~%")
  (init)
  (window-canvas:remove-all-windows)
  (window-canvas:window-main (make-instance 'boxes-window
                                            :width  window-canvas:*gtk-initial-window-width*
                                            :height window-canvas:*gtk-initial-window-height*)))

;;; testing ====================================================================
(defun experiment-init ()
  (format T "boxes main~%")
  (init)
  (window-canvas::window-creation-from-simulation :testing
                                                  (make-instance 'boxes-window
                                                                 :width  window-canvas:*gtk-initial-window-width*
                                                                 :height window-canvas:*gtk-initial-window-height*)))

;; (experimental-run)
(defun experimental-run ()
  (experiment-init)
  (let ((window (window-canvas:get-window :testing)))
    (process-gtk-event window :resize '(400 500))
    (process-gtk-event window :timeout)
    (process-gtk-event window :resize '(500 600))
    (process-gtk-event window :key-pressed '("" "Right" 114 NIL))
    (process-gtk-event window :key-pressed '("w" "w" 114 NIL))
    (process-gtk-event window :key-pressed '("w" "w" 114 NIL))
    (process-gtk-event window :key-pressed '("w" "w" 114 NIL))
    (process-gtk-event window :key-pressed '("w" "w" 114 NIL))
    (process-gtk-event window :key-pressed '("w" "w" 114 NIL))

    (process-gtk-event window :key-pressed '("h" "h" 114 NIL))
    (process-gtk-event window :key-pressed '("h" "h" 114 NIL))
    (process-gtk-event window :key-pressed '("h" "h" 114 NIL))
    (process-gtk-event window :key-pressed '("h" "h" 114 NIL))
    (process-gtk-event window :key-pressed '("h" "h" 114 NIL))
    (process-gtk-event window :key-pressed '("h" "h" 114 NIL))
    (process-gtk-event window :key-pressed '("h" "h" 114 NIL))
    ))

(main)
