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
    (cairo:show-text (format nil "~A" my-text))))

;;; === events =================================================================
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
    (otherwise
     (warn "not handled event ~S ~S" event args)))

  (maphash (lambda (key lwin)
             (declare (ignore key))
             (gui-window:redraw-canvas lwin))
           (gui-window:all-windows)))

;;; === main ===================================================================
(defun main ()
  (setf gui-window:*client-fn-menu-bar*      'counter::menu-bar
        gui-window:*client-fn-draw-objects*  'counter::draw-window
        gui-events:*client-fn-process-event* 'counter::process-event
        gui-window:*initial-window-width*    600
        gui-window:*initial-window-height*   400
        gui-window:*initial-title*           "Counter")

  (gui-window:window (make-instance 'gui-window:lisp-window)))

(main)
