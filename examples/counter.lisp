(declaim (optimize (speed 0) (safety 2) (debug 3)))

;; (load "~/Programming/Lisp/clops-gui/examples/counter.lisp")

;;; === load gui system ========================================================
(push #p "~/Programming/Lisp/clops-gui" ql:*local-project-directories*)
(ql:quickload '(clops-gui) :silent nil)

;;; === package ================================================================
(defpackage #:counter
  (:use #:cl)
  (:import-from :serapeum
                ~>)
  (:import-from :defclass-std
                :defclass/std))

(in-package #:counter)

(defparameter *model*  nil)
(defparameter *window* nil)

(defclass/std model ()
  ((counted)))

(defclass/std counter-window (gui-window:lisp-window)
  ((button-plus)
   (label-counted)
   (button-minus)
   (button-reset)))

(defmethod initialize-instance :after ((model model) &rest initargs &key)
  (declare (ignore initargs))
  (setf (counted model) 0))

(defmethod initialize-instance :after ((window counter-window) &rest initargs &key)
  (declare (ignore initargs))
  (with-accessors ((button-plus button-plus)
                   (label-counted label-counted)
                   (button-minus button-minus)
                   (button-reset button-reset))
      window
    (progn
      (setf button-plus    (make-instance 'gui-box:text-box
                                          :top-left (make-instance 'gui-box:coordinates :x 20 :y 100)
                                          :width 50 :height 20 :text "+"))
      (gui-window:add-child window button-plus)

      (setf label-counted  (make-instance 'gui-box:text-box
                                          :top-left (make-instance 'gui-box:coordinates :x 100 :y 100)
                                          :width 50 :height 20 :text (counted *model*)))
      (gui-window:add-child window label-counted)

      (setf button-minus   (make-instance 'gui-box:text-box
                                          :top-left (make-instance 'gui-box:coordinates :x 180 :y 100)
                                          :width 50 :height 20 :text "-"))
      (gui-window:add-child window button-minus)

      (setf button-reset   (make-instance 'gui-box:text-box
                                          :top-left (make-instance 'gui-box:coordinates :x 260 :y 100)
                                          :width 50 :height 20 :text "Reset"))
      (gui-window:add-child window button-reset))))



(defmethod inc ((model model))
  (incf (counted mode)))

(defmethod dec ((model model))
  (decf (counted model)))

(defmethod reset ((model model))
  (setf (counted model) 0))

(defmethod render ((box gui-box:text-box))
  (gui-window:set-rgba (if (gui-box::mouse-overp box)
                           (if (zerop (~> gui-window:*lisp-app* gui-window:mouse-button))
                               "yellow"
                               "red")
                           "green"))
  (cairo:rectangle
   (~> box gui-box:top-left gui-box:x)
   (~> box gui-box:top-left gui-box:y)
   (~> box gui-box:width)
   (~> box gui-box:height))
  (cairo:fill-path)

  (cairo:select-font-face "Ubuntu Mono" :normal :bold)
  (cairo:set-font-size 20)

  (cairo:move-to (~> box gui-box:top-left gui-box:x)
                 (~> box gui-box:top-left gui-box:y))
  (gui-window:set-rgba "black")
  (cairo:show-text (format nil "~A" (~> box gui-box:text))))

;;; === menu declaration =======================================================
(defun menu-bar (app lisp-window)
  (let ((menu (gio:make-menu)))
    (gui-menu:build-menu
     menu

     (gui-menu:prepare-submenu
      "File"

      (gui-menu:prepare-section
       nil
       (gui-menu:build-items
        (gui-menu:prepare-item-simple lisp-window app menu "Quit" "quit")))
      ;; end of prepare-submenu File
      )

     (gui-menu:prepare-submenu
      "Help"

      (gui-menu:prepare-section
       nil
       (gui-menu:build-items
        (gui-menu:prepare-item-simple lisp-window app menu "About" "about")))
      ;; end of prepare-submenu Help
      )
     ;; end of build-menu
     )

    (values menu)))

;;; === drawing ================================================================
(defmethod draw-window ((window gui-window:lisp-window))
  (cairo:set-source-rgb  1 1 1)
  (cairo:paint)

  (cairo:select-font-face "Ubuntu Mono" :normal :bold)
  (cairo:set-font-size 20)

  (let ((my-text "New project will go here"))
    (multiple-value-bind  (xb yb width height)
        (cairo:text-extents my-text)
      (declare (ignore xb yb width height)))
    (cairo:set-source-rgb 0 0 0)
    (cairo:move-to 20 20)
    (cairo:show-text (format nil "~A" my-text)))

  (render (button-plus window))
  (render (button-minus window))
  (render (button-reset window))
  (render (label-counted window)))

;;; === events =================================================================
(defmethod press-box ((box gui-box:text-box))
  (cond ((equal (gui-box:text box) "+")
         (incf (counted *model*)))
        ((equal (gui-box:text box) "-")
         (decf (counted *model*)))
        ((equal (gui-box:text box) "Reset")
         (setf (counted *model*) 0))))

(defmethod process-event ((lisp-window gui-window:lisp-window) event &rest args)
  (case event
    (:timeout
     ;; do nothing yet
     )
    (:menu-simple
          (destructuring-bind ((menu-item)) args
            (warn "menu item ~s" menu-item)
            (cond
              ((equal menu-item "quit")
               (gui-window:close-all-windows-and-quit))

              ((equal menu-item "about")
               (gui-window:present-about-dialog
                (list
                 :authors (list "Jacek Podkanski")
                 :website "https://github.com/bigos"
                 :program-name "Counter"
                 :comments "Nothing to say yet"
                 :license "Public Domain"
                 :system-information (format nil "~A" (uiop/os:implementation-identifier))
                 :logo-icon-name "application-x-addon")))
              (T
               (warn "not processed event ~S ~S" event args)))))
    (:key-pressed
     (destructuring-bind ((entered key-name key-code mods)) args
       (format t "~&>>> key pressed ~S~%" (list entered key-name key-code mods))))
    ((:motion :motion-enter)
     (destructuring-bind ((x y)) args
       (setf (gui-window:mouse-coordinates gui-window:*lisp-app*) (cons x y)
             (gui-window:current-motion    gui-window:*lisp-app*) lisp-window)))
    (:motion-leave
     (setf (gui-window:mouse-coordinates gui-window:*lisp-app*) nil
           (gui-window:current-motion gui-window:*lisp-app*) nil))
    (:pressed
     ;; TODO find better way of finding mouse buttons state
     (destructuring-bind ((button x y)) args
       (declare (ignore x y))
       (incf (gui-window:mouse-button gui-window:*lisp-app*) (expt 2 button))
       (loop for c in (gui-window:children lisp-window)
             do (if (gui-box::mouse-overp c)
                    (press-box c))))
     (warn "button after press ~S" (gui-window:mouse-button gui-window:*lisp-app*)))
    (:released
     (destructuring-bind ((button x y)) args
       (declare (ignore button x y))
       (setf (gui-window:mouse-button gui-window:*lisp-app*) 0))
     (warn "button after release ~S" (gui-window:mouse-button gui-window:*lisp-app*)))
    (:resize
     (destructuring-bind ((w h)) args
       (gui-window:window-resize w h lisp-window)))
    (otherwise
     (warn "not handled event ~S ~S" event args)))

  (maphash (lambda (key lwin)
             (declare (ignore key))
             (gui-window:redraw-canvas lwin))
           (gui-window:all-windows)))

;;; === main ===================================================================
(defun init ()
  (setf
   *model*  (make-instance 'model)
   *window* (make-instance 'counter-window)))

(defun main ()
  (init)

  (setf gui-window:*client-fn-menu-bar*      'counter::menu-bar
        gui-window:*client-fn-draw-objects*  'counter::draw-window
        gui-events:*client-fn-process-event* 'counter::process-event
        gui-window:*initial-window-width*    600
        gui-window:*initial-window-height*   400
        gui-window:*initial-title*           "Counter")

  (gui-window:window *window*))

(main)
