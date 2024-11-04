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
(defparameter *model* nil)
(defclass/std counter-second-window (gui-window:lisp-window) (()))

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
  (cairo:show-text (format nil "~A" (access:accesses *model* '(:counter :type :alist))))

  (gui-window:set-rgba "yellow")
  (cairo:rectangle (access:accesses *model* '(:txa :type :alist))
                   (access:accesses *model* '(:tya :type :alist))
                   (-
                    (access:accesses *model* '(:bxa :type :alist))
                    (access:accesses *model* '(:txa :type :alist)))
                   (-
                    (access:accesses *model* '(:bya :type :alist))
                    (access:accesses *model* '(:tya :type :alist))))
  (cairo:fill-path)


  (gui-window:set-rgba "orange")
  (cairo:rectangle (access:accesses *model* '(:txb :type :alist))
                   (access:accesses *model* '(:tyb :type :alist))
                   (-
                    (access:accesses *model* '(:bxb :type :alist))
                    (access:accesses *model* '(:txb :type :alist)))
                   (-
                    (access:accesses *model* '(:byb :type :alist))
                    (access:accesses *model* '(:tyb :type :alist))))
  (cairo:fill-path))

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
       (gui-app:mouse-motion-enter lisp-window x y)))
    (:motion-leave
     (gui-app:mouse-motion-leave))
    (:focus-enter)
    (:focus-leave)
    (:pressed
     (destructuring-bind ((button x y)) args
       (declare (ignore button x y))
       ))
    (:released
     (gui-app:mouse-button-released))
    (:scroll)
    (:resize
     (destructuring-bind ((w h)) args
       (gui-window:window-resize w h lisp-window)))
    (:key-pressed
     (destructuring-bind ((entered key-name key-code mods)) args
       (format t "~&>>> key pressed ~S~%" (list entered key-name key-code mods))
       ))
    (otherwise
     (warn "not handled event ~S ~S" event args)))

  ;; moving widgets -------------------------
  ;; (warn "may implement moving widgets in response to actions)
  ;; redrawing ------------------------------
  (gui-window:redraw-canvas lisp-window (format  nil "~A" event)))

;;; main =======================================================================
(defun main ()
  ;; initialize model
  (setf
   (access:accesses *model* '(:counter :type :alist)) 0
   (access:accesses *model* '(:txa :type :alist)) 10
   (access:accesses *model* '(:tya :type :alist)) 200
   (access:accesses *model* '(:bxa :type :alist)) 60
   (access:accesses *model* '(:bya :type :alist)) 220

   (access:accesses *model* '(:txb :type :alist)) 110
   (access:accesses *model* '(:tyb :type :alist)) 200
   (access:accesses *model* '(:bxb :type :alist)) 160
   (access:accesses *model* '(:byb :type :alist)) 220

   )

  (setf
   gui-drawing:*client-fn-draw-objects*  'counter-second::draw-window
   gui-window-gtk:*client-fn-menu-bar*      nil
   gui-events:*client-fn-process-event* 'counter-second::process-event
   gui-window-gtk:*initial-window-width*    600
   gui-window-gtk:*initial-window-height*   400
   gui-window-gtk:*initial-title*           "Counter Second")

  (gui-window-gtk:window (make-instance 'counter-second-window)))

(main)
