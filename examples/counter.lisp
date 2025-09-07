(declaim (optimize (speed 0) (safety 3) (debug 3)))

;;;; Example counter

;;; load ===================================================================
;; (load "~/Programming/Lisp/clops-gui/examples/counter.lisp")

(ql:quickload '(:clops-gui) :silent nil)

;;; package ================================================================
(defpackage #:counter
  (:use #:cl)
  (:import-from :serapeum
   :~>)
  (:import-from :defclass-std
   :defclass/std)
  (:import-from
   :boxes
   ;; ---
   :*model*
   :make-coordinates-relative
   :make-coordinates-absolute
   :make-model
   :make-node
   :make-node-left
   :make-node-right
   :make-node-up
   :make-node-down
   :make-node-auto
   :add-children
   :node-text
   :node-character
   :bbx
   :bby
   :grw
   :grh
   :tbw
   :mouse-position
   :mouse-over-p
   :text
   :wrap
   :width
   :height
   :render
   :boxes-window))

(in-package #:counter)

;;; minimal window -------------------------------------------------------------


(defclass/std counter-window (boxes::boxes-window) (()))

(defparameter b-plu (make-instance
                     'node-text
                     :coordinates-relative (make-coordinates-relative 50 200)
                     :width 50
                     :height 50
                     :color "red"
                     :wrap 'truncate
                     :text "Plus"))

(defparameter b-eq (make-instance
                    'node-text
                    :coordinates-relative (make-coordinates-relative 150 200)
                    :width 150
                    :height 50
                    :color "orange"
                    :wrap 'truncate
                    :text "0"))

(defparameter b-min (make-instance
                     'node-text
                     :coordinates-relative (make-coordinates-relative 350 200)
                     :width 50
                     :height 50
                     :color "yellow"
                     :wrap 'truncate
                     :text "Minus"))

(defparameter counter 0)

;;; drawing ====================================================================
(defmethod draw-window ((window counter-window))
  ;; paint background
  (let ((cv 0.95)) (cairo:set-source-rgb  cv cv cv))
  (cairo:paint)

  (progn
    (cairo:select-font-face "Ubuntu Mono" :italic :bold)
    (cairo:set-font-size 10)
    (cairo:move-to 10 10)
    (gui-window:set-rgba "black")
    (cairo:show-text (format nil "try moving the mouse over the window and outside of it")))


  (cairo:select-font-face "Ubuntu Mono" :italic :bold)
  (cairo:set-font-size 15)
  (cairo:move-to 10 100)
  (let ((cmotion    (gui-app:current-motion-window-p gui-app:*lisp-app* window)))
    (if cmotion
        (gui-window:set-rgba "green")
        (gui-window:set-rgba "red"))
    (cairo:show-text (format nil "motion ~A" cmotion)))

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
      (cairo:fill-path)))
  ;; ==================================================================

  (let ((world (make-node 0 0 (width window) (height window) "#cccccc88")))

    (boxes:absolute-coordinates world)
    (setf (text b-eq) (format nil "~S" counter))

    (add-children world
                  (list
                   ;; for the counter we do not need those examples
                   ;; (add-children (make-node 10
                   ;;                          10
                   ;;                          50 50 "purple")
                   ;;               (list
                   ;;                (make-node 10 10 25 25 "violet")))
                   (add-children b-plu  nil)
                   (add-children b-eq   nil)
                   (add-children b-min  nil)))

    ;; (warn "adding absolute coordinates -----------------------------------")
    (boxes:absolute-coordinates world)

    ;; (warn "rendering -----------------------------------------------")
    (render world)))

;;; events =====================================================================
(defmethod process-event ((lisp-window counter-window) event &rest args)
  (unless (eq event :timeout)
    (warn "event ~S ~S" event args))

  (case event
    (:timeout
     ;; do nothing yet
     )
    ((:motion :motion-enter)
     ;; we use simple case with one window so we ignore the window argument
     (destructuring-bind ((x y)) args
       (setf (mouse-position *model*) (cons x y))
       (gui-app:mouse-motion-enter lisp-window x y)))
    (:motion-leave
     (gui-app:mouse-motion-leave))
    (:focus-enter)
    (:focus-leave)
    (:pressed
     (destructuring-bind ((button x y)) args
       (declare (ignore button))

       (cond
         ((mouse-over-p b-plu)
          (incf counter)
          (warn "b-plu ~S" counter))
         ((mouse-over-p b-eq)
          (warn "b-eq"))
         ((mouse-over-p b-min)
          (decf counter)
          (warn "b-min ~s" counter))
         (T (warn "no button")))

       ))
    (:released
     (gui-app:mouse-button-released))
    (:scroll)
    (:resize
     (destructuring-bind ((w h)) args
       (gui-window:window-resize w h lisp-window)
       (setf (width lisp-window) w
             (height lisp-window) h)))
    (:key-pressed
     (destructuring-bind ((entered key-name key-code mods)) args
       (format t "~&>>> key pressed ~S~%" (list entered key-name key-code mods))
       ))
    (otherwise
     (warn "not handled event ~S ~S" event args)))

  (gui-window:redraw-canvas lisp-window (format  nil "~A" event)))

;;; main =======================================================================
(defun init ()
  (setf
   gui-drawing:*client-fn-draw-objects*  'counter::draw-window
   gui-window-gtk:*client-fn-menu-bar*      nil
   gui-events:*client-fn-process-event* 'counter::process-event
   gui-window-gtk:*initial-window-width*    600
   gui-window-gtk:*initial-window-height*   400
   gui-window-gtk:*initial-title*           "Counter"
   *model* (make-model)))

(defun main ()
  (init)
  (gui-window-gtk:window-main (make-instance 'counter-window)))

(main)

;;; testing ====================================================================
(defun experiment-init ()
  (init)
  (let ((experimental-window (make-instance 'counter-window
                                            :width  gui-window-gtk:*initial-window-width*
                                            :height gui-window-gtk:*initial-window-height*)))
    (setf gui-app:*lisp-app* (gui-app:make-lisp-app nil))
    (setf (gui-window::gir-window experimental-window) :testing)

    (gui-window-gtk:window-creation-from-simulation :testing experimental-window)
    experimental-window))

;;    (experimental-run)
(defun experimental-run ()
  (let ((window (experiment-init)))
    (process-event window :resize '(600 500))
    (process-event window :timeout)
    (process-event window :resize '(500 600))
    ))
