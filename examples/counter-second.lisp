(declaim (optimize (speed 0) (safety 3) (debug 3)))

;;;; Example of counter-second window

;;; load ===================================================================
;; (load "~/Programming/Lisp/clops-gui/examples/counter-second.lisp")
(push #p "~/Programming/Lisp/clops-gui/" ql:*local-project-directories*)
(ql:quickload '(:clops-gui :access) :silent T)

;;; package ================================================================
(defpackage #:counter-second
  (:use #:cl)
  (:import-from :serapeum
                :~>)
  (:import-from :defclass-std
                :defclass/std))

(in-package #:counter-second)

;;; code =======================================================================
(defstruct model
  (counted 0)
  (a-mouseover nil)
  (b-mouseover nil)
  (txa nil)
  (tya nil)
  (bxa nil)
  (bya nil)
  (txb nil)
  (tyb nil)
  (bxb nil)
  (byb nil))

(defparameter *model* nil)
(defclass/std counter-second-window (gui-window:lisp-window) (()))

;;; model functions
(defun draw-rectangle-a (model color)
  (gui-window:set-rgba
   (if (model-a-mouseover model)
       "red"
       color))
  (cairo:rectangle (model-txa model)
                   (model-tya model)
                   (- (model-bxa model) (model-txa model))
                   (- (model-bya model) (model-tya model)))
  (cairo:fill-path))

(defun draw-rectangle-b (model color)
  (gui-window:set-rgba
   (if (model-b-mouseover model)
       "red"
       color))
  (cairo:rectangle (model-txb model)
                   (model-tyb model)
                   (- (model-bxb model) (model-txb model))
                   (- (model-byb model) (model-tyb model)))

  (cairo:fill-path))

(defun mouse-overp (model x y id)
  (ecase id
    (:a (and (>= x (model-txa model))
             (<= x (model-bxa model))
             (>= y (model-tya model))
             (<= y (model-bya model))))

    (:b (and (>= x (model-txb model))
             (<= x (model-bxb model))
             (>= y (model-tyb model))
             (<= y (model-byb model))))))

(defun update-mouse-location (model x y)
  (when (or (null x) (null y)) (error "null coordinates are not acceptable"))
  (if (mouse-overp model x y :a)
      (update-mouse-over model :a)
      (update-mouse-out model :a))

  (if (mouse-overp model x y :b)
      (update-mouse-over model :b)
      (update-mouse-out model :b)))

(defun update-mouse-press (model x y button)
  (when (model-a-mouseover model)
    (incf (model-counted model)))

  (when (model-b-mouseover model)
    (decf (model-counted model))))

(defun update-mouse-release (model))
(defun update-mouse-over (model id)
  (ecase id
    (:a (setf (model-a-mouseover model) t))
    (:b (setf (model-b-mouseover model) t))))

(defun update-mouse-out (model id)
  (ecase id
    (:a (setf (model-a-mouseover model) nil))
    (:b (setf (model-b-mouseover model) nil))))

;;; === experiment ==============================================================
(defun experiment-first-window ()
  (setf gui-drawing:*client-fn-draw-objects*  'counter-second::draw-window)

  (setf gui-app:*lisp-app* (gui-app:make-lisp-app))
  (assert (zerop (hash-table-count (gui-app:all-windows))))

  (let ((lisp-window (make-instance 'counter-second-window)))
    (gui-window-gtk:window-creation-from-simulation :testing lisp-window)
    (assert (eq 1 (hash-table-count (gui-app:all-windows))))
    lisp-window))

(defun experiment ()
  "Experiment for testing"
  (warn "starting experiments")
  (init-model)
  (let ((lisp-window (experiment-first-window))
        (events '((:RESIZE ((600 400))) (:KEY-RELEASED (("" "Return" 36 NIL)))
                  (:TIMEOUT (NIL)) (:MOTION-ENTER ((194.0d0 390.0d0)))
                  (:MOTION ((39.4 210.1)))
                  (:assert (zerop (model-counted *model*)))
                  (:PRESSED ((1 39.4 210.1)))
                  (:RELEASED ((1 39.4 210.1)))
                  (:PRESSED ((1 39.4 210.1)))
                  (:RELEASED ((1 39.4 210.1)))
                  (:assert (eq 1 (model-counted *model*)))

             )))
    (loop for event in events
          for e = (car event)
          for eargs = (caadr event)
          do
             (break "data ~s" (list
                               gui-app:*lisp-app*
                               lisp-window
                               event))
             (if (eq e :assert)
                 (assert (eval (cadr event)))
                 (funcall 'process-event
                          lisp-window
                          e
                          eargs ))))
  (warn "finished experiments"))

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
  (cairo:show-text (format nil "~A" (model-counted *model*)))

  (draw-rectangle-a *model* "yellow")
  (draw-rectangle-b *model* "orange"))

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
  (setf *model* (make-model
                 :txa 10 :tya 200
                 :bxa 60 :bya 220
                 :txb 110 :tyb 200
                 :bxb 160 :byb 220)))

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
