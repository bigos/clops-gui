(declaim (optimize (speed 0) (safety 3) (debug 3)))

;;;; Example of counter-second window

;; https://common-lisp-libraries.readthedocs.io/fiveam/

;;; load ===================================================================
;; (load "~/Programming/Lisp/clops-gui/examples/counter-second.lisp")
(push #p "~/Programming/Lisp/clops-gui/" ql:*local-project-directories*)
(ql:quickload '(:clops-gui :access :fiveam) :silent nil)

;;; package ================================================================
(defpackage #:counter-second
  (:use #:cl)
  (:import-from :serapeum
                :~>)
  (:import-from :defclass-std
                :defclass/std))

(in-package #:counter-second)

;;; code =======================================================================
(defparameter *model* nil)

(defclass/std model ()
  ((counted :std 0)
   (a-mouseover :std nil)
   (b-mouseover :std nil)
   (rect-a  :type rect)
   (rect-b  :type rect)))

(defclass/std counter-second-window (gui-window:lisp-window) (()))

(defclass/std rect ()
  ((tx)
   (ty)
   (bx)
   (by)))

;;; model functions

(defun draw-rectangle (model id color)
  (gui-window:set-rgba (if (or (and (a-mouseover model) (eq id :a))
                               (and (b-mouseover model) (eq id :b)))
                           "red"
                           color))

  (let ((rect (ecase id
                (:a (~> model rect-a))
                (:b (~> model rect-b)))))
    (cairo:rectangle (~> rect tx)
                     (~> rect ty)
                     (~> rect width)
                     (~> rect height)))
  (cairo:fill-path))

(defun mouse-overp (model x y id)
  (let ((rect (ecase id
                (:a (~> model rect-a))
                (:b (~> model rect-b)))))
    (and (>= x (~> rect tx))
         (<= x (~> rect bx))
         (>= y (~> rect ty))
         (<= y (~> rect by)))))

(defun update-mouse-location (model x y)
  (when (or (null x) (null y)) (error "null coordinates are not acceptable"))
  (if (mouse-overp model x y :a)
      (set-mouseover model :a)
      (set-mouse-out model :a))

  (if (mouse-overp model x y :b)
      (set-mouseover model :b)
      (set-mouse-out model :b)))

(defun update-mouse-press (model x y button)
  (when (a-mouseover model)
    (incf (counted model)))

  (when (b-mouseover model)
    (decf (counted model))))

(defun update-mouse-release (model))

(defun set-mouseover (model id)
  (ecase id
    (:a (setf (a-mouseover model) t))
    (:b (setf (b-mouseover model) t))))

(defun set-mouse-out (model id)
  (ecase id
    (:a (setf (a-mouseover model) nil))
    (:b (setf (b-mouseover model) nil))))

(defmethod width ((rect rect))
  (- (~> rect bx) (~> rect tx)))

(defmethod height ((rect rect))
  (- (~> rect by) (~> rect ty)))

;;; === test preparation =======================================================
(defun experiment-first-window ()
  (setf gui-drawing:*client-fn-draw-objects*  'counter-second::draw-window)

  (setf gui-app:*lisp-app* (gui-app:make-lisp-app))
  (assert (zerop (hash-table-count (gui-app:all-windows))))

  (let ((lisp-window (make-instance 'counter-second-window)))
    (gui-window-gtk:window-creation-from-simulation :testing lisp-window)
    (assert (eq 1 (hash-table-count (gui-app:all-windows))))
    lisp-window))

;;; drawing ====================================================================
(defmethod draw-window ((window counter-second-window))
  ;; paint background
  (let ((cv 0.95)) (cairo:set-source-rgb  cv cv cv))
  (cairo:paint)

  (cairo:select-font-face "Ubuntu Mono" :italic :bold)
  (cairo:set-font-size 10)
  (cairo:move-to 10 10)
  (gui-window:set-rgba "black")
  (cairo:show-text (format nil "try moving the mouse over the window and outside of it"))

  (cairo:select-font-face "Ubuntu Mono" :italic :bold)
  (cairo:set-font-size 15)
  (cairo:move-to 10 100)
  (let ((cmotion (gui-app:current-motion-window-p gui-app:*lisp-app* window)))
    (if cmotion
        (gui-window:set-rgba "green")
        (gui-window:set-rgba "red"))
    (cairo:show-text (format nil "motion ~A" cmotion)))

  (cairo:set-font-size 30)
  (cairo:move-to 10 150)
  (gui-window:set-rgba "blue")
  (cairo:show-text (format nil "~A" (counted *model*)))

  (draw-rectangle *model* :a "yellow")
  (draw-rectangle *model* :b "orange"))

(defmethod draw-window :after ((window counter-second-window))
  ;; pink square follows the mouse
  (let ((app gui-app:*lisp-app*))
    (when (and (eq (gui-app:current-motion app)
                   window)
               (gui-app:mouse-coordinates app))
      (gui-window:set-rgba "pink")
      (cairo:rectangle
       (car (gui-app:mouse-coordinates app))
       (cdr (gui-app:mouse-coordinates app))
       25
       25)
      (cairo:fill-path))))

;;; events =====================================================================
(defmethod process-event ((lisp-window counter-second-window) event &rest args)
  (unless (member event '(:timeout :motion))
    (warn "event ~S ~S" event args))

  (case event
    (:timeout
     ;; do nothing yet
     )
    ((:motion :motion-enter)
     ;; we use simple case with one window so we ignore the window argument
     (destructuring-bind ((x y)) args
       (update-mouse-location *model* x y)
       (gui-app:mouse-motion-enter lisp-window x y)))
    (:motion-leave
     (gui-app:mouse-motion-leave))
    (:focus-enter)
    (:focus-leave)
    (:pressed
     (destructuring-bind ((button x y)) args
       (declare (ignore button x y))
       (update-mouse-press *model* x y button)
       ))
    (:released
     (update-mouse-release *model*)
     (gui-app:mouse-button-released))
    (:scroll)
    (:resize
     (destructuring-bind ((w h)) args
       (gui-window:window-resize w h lisp-window)))
    (:key-pressed
     (destructuring-bind ((entered key-name key-code mods)) args
       (format t "~&>>> key pressed ~S~%" (list entered key-name key-code mods))
       (warn "model ~S" *model*)
       ))
    (otherwise
     (warn "not handled event ~S ~S" event args)))

  ;; moving widgets -------------------------
  ;; (warn "may implement moving widgets in response to actions)

  ;; redrawing ------------------------------
  (gui-window:redraw-canvas lisp-window (format  nil "~A" event)))

;;; main =======================================================================
(defun init-model ()
  (setf *model* (make-instance 'model
                 :rect-a (make-instance 'rect :tx 10  :ty 200 :bx 60  :by 220)
                 :rect-b (make-instance 'rect :tx 110 :ty 200 :bx 160 :by 220))))

(defun main ()
  (init-model)

  (setf
   gui-drawing:*client-fn-draw-objects*  'counter-second::draw-window
   gui-window-gtk:*client-fn-menu-bar*      nil
   gui-events:*client-fn-process-event* 'counter-second::process-event
   gui-window-gtk:*initial-window-width*    600
   gui-window-gtk:*initial-window-height*   400
   gui-window-gtk:*initial-title*           "Counter Second")

  (gui-window-gtk:window (make-instance 'counter-second-window)))

;; (main)

;;; !!!!!!!!!!!!!!!!!!!!!!!!!!testing!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
(in-package :common-lisp-user)

(defpackage #:counter-second.test
  (:use #:cl  #:counter-second #:fiveam)
  (:import-from #:counter-second
   :*model*
   :a-mouseover
   :b-mouseover
   :counted
   :experiment-first-window
   :init-model
   :model
   :process-event
   )
  (:export #:run!))

(in-package #:counter-second.test)

(defparameter lisp-window (experiment-first-window))

(defparameter a-mouse-coordinates         '(39.4 210.1))
(defparameter a-mouse-coordinates-press '(1 39.4 210.1))

(defparameter b-mouse-coordinates         '(125.4 210.1))
(defparameter b-mouse-coordinates-press '(1 125.4 210.1))

(defun doit (e eargs)
  (process-event lisp-window
                 e
                 eargs))

(def-suite my-tests :description "my tests")

(in-suite my-tests)

(test two-and-two
      (is (eq (+ 2 2) 4)))

(test model-creation
  (setf *model* nil)
  (let ((m (init-model)))
    (is (eq (type-of m) 'model))))

(test counter-clicking
      (setf *model* nil)
      (init-model)

      (doit :RESIZE '(600 400))
      (doit :KEY-RELEASED '("" "Return" 36 NIL))
      (doit :TIMEOUT NIL)
      (is (null (a-mouseover *model*)))
      (is (null (b-mouseover *model*)))

      (doit :MOTION-ENTER '(194.0d0 390.0d0))
      (doit :MOTION a-mouse-coordinates)
      (is (zerop (counted *model*)))
      (is (not (null (a-mouseover *model*))))
      (is      (null (b-mouseover *model*)))

      (doit :PRESSED  a-mouse-coordinates-press)
      (doit :RELEASED a-mouse-coordinates-press)
      (doit :PRESSED  a-mouse-coordinates-press)
      (doit :RELEASED a-mouse-coordinates-press)
      (is (eq 2 (counted *model*)))
      (is (not (null (a-mouseover *model*))))
      (is      (null (b-mouseover *model*)))

      (doit :MOTION     b-mouse-coordinates)
      (is      (null (a-mouseover *model*)))
      (is (not (null (b-mouseover *model*))))

      (doit :PRESSED  b-mouse-coordinates-press)
      (doit :RELEASED b-mouse-coordinates-press)

      (is (eq 1 (counted *model*))))

(run! 'my-tests)
