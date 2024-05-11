(declaim (optimize (speed 0) (safety 2) (debug 3)))

;; (load "~/Programming/Lisp/clops-gui/examples/pong.lisp")

(defpackage #:pong
  (:use #:cl))

(in-package #:pong)
;;; === load ===================================================================
(push #p "~/Programming/Lisp/clops-gui/" ql:*local-project-directories*)
(ql:quickload '(:clops-gui) :silent nil)

;;; === defgenerics ============================================================

;;; === classes ================================================================

;;; === methods ================================================================

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
        (gui-menu:prepare-item-simple lisp-window app menu "Restart" "restart")))
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
        (gui-menu:prepare-item-simple lisp-window app menu "Tutorial" "tutorial")))
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

  (let ((my-text "Pong will go here"))
    (multiple-value-bind  (xb yb width height)
        (cairo:text-extents my-text)
      (declare (ignore xb yb width height)))
    (cairo:set-source-rgb 0 0 0)
    (cairo:move-to 20 20)
    (cairo:show-text (format nil "~A" my-text))))

;;; === events =================================================================
(defmethod process-event ((lisp-window gui-window:lisp-window) event &rest args)
  (unless (member event '(:timeout :motion))
    (format t "~&going to process ~A ~A  "
            event
            (case event
              ((:focus-enter :focus-leave)
               (gui-window:window-hkey lisp-window))
              (:key-pressed
               (destructuring-bind ((letter name code mods)) args
                 (warn "pressed ~S" (list letter name code mods (gui-window:window-hkey lisp-window)))))
              (T args))))

  (case event
    (:timeout
     ;; do nothing yet
     )
    (otherwise
     (warn "not handled event ~S ~S" event args)))

  (maphash (lambda (key lwin)
             (declare (ignore key))
             (gui-window:redraw-canvas lwin))
           (gui-window:all-windows)))

;;; === main ===================================================================
(defun main ()
  (setf gui-window:*client-fn-menu-bar*      'pong::menu-bar
        gui-window:*client-fn-draw-objects*  'pong::draw-window
        gui-events:*client-fn-process-event* 'pong::process-event
        gui-window:*initial-window-width*    600
        gui-window:*initial-window-height*   400
        gui-window:*initial-title*           "Pong")

  (gui-window:window (make-instance 'gui-window:lisp-window)))

(main)
