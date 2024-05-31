(declaim (optimize (speed 0) (safety 2) (debug 3)))

;; (load "~/Programming/Lisp/clops-gui/examples/pong.lisp")

;;; === load ===================================================================
(push #p "~/Programming/Lisp/clops-gui/" ql:*local-project-directories*)
(ql:quickload '(:clops-gui) :silent nil)

;;; === package ================================================================
(defpackage #:pong
  (:use #:cl)
  (:import-from :serapeum
   :~>)
  (:import-from :defclass-std
   :defclass/std))

(in-package #:pong)

(defparameter *pong-game* nil)

;;; ======================= experiment =========================================
(defun experiment-first-window ()
  (setf
   gui-window:*client-fn-draw-objects*  'pong::draw-window
   gui-window:*lisp-app* (make-instance 'gui-window::lisp-app))

  (assert (zerop (hash-table-count (gui-window:all-windows))))
  (let ((lisp-window (make-instance 'pong-window)))
    (gui-window::window-creation-from-simulation :testing lisp-window)
    (assert (eq 1 (hash-table-count (gui-window:all-windows))))
    lisp-window))

(defun experiment ()
  (warn "starting experiment")

  (let ((lisp-window (experiment-first-window)))
    (process-event lisp-window :resize (list 600 200 ))
    (process-event lisp-window :timeout)
    (process-event lisp-window :motion-enter (list 50 50 ))

    (process-event lisp-window :key-pressed (list "" "Escape" 9  nil))
    (process-event lisp-window :key-pressed (list "" "F1" 67     nil))
    (process-event lisp-window :key-pressed (list "r" "r" 10     nil))
    (process-event lisp-window :key-pressed (list " " "space" 65 nil))))

;;; === defgenerics ============================================================

;;; === classes ================================================================

;;; === methods ================================================================

;;; --- rendering -------------------------------

(defmethod render ((anything T))
  (warn "@@@ handling weird render @@@ ~S" (list (type-of anything) anything))
  (break "zzzzzzzzzzzzzzzzzzzz ~A" anything))

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

(defmethod draw-window ((window T))
  (progn
    (cairo:set-source-rgb  1 1 1)
    (cairo:paint)

    (cairo:select-font-face "Ubuntu Mono" :normal :bold)
    (cairo:set-font-size 20)

    (let ((my-text "Huh?"))
      (multiple-value-bind  (xb yb width height)
          (cairo:text-extents my-text)
        (declare (ignore xb yb width height)))
      (cairo:set-source-rgb 0 0 0)
      (cairo:move-to 20 20)
      (cairo:show-text (format nil "~A" my-text)))))

;;; === events =================================================================

(defmethod process-event ((lisp-window lisp-window) event &rest args)
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
    (:timeout)
    (:menu-simple
     (destructuring-bind ((menu-item)) args
       (warn "menu item ~s" menu-item)
       (cond
         ((equal menu-item "restart")
          ;; start the game
          (restart-helper lisp-window))

         ((equal menu-item "quit")
          (gui-window:close-all-windows-and-quit))

         ((equal menu-item "tutorial")
          (show-tutorial-helper))

         ((equal menu-item "about")
          (gui-window:present-about-dialog
           (list
            :authors (list "Jacek Podkanski")
            :website "https://github.com/bigos/clops-gui"
            :program-name "Pong"
            :comments "Pong example written for cl-gtk4"
            :license "Public Domain"
            :system-information (format nil "~A"
                                        (uiop/os:implementation-identifier))
            :logo-icon-name "application-x-addon")))

         (T
          (warn "not processed event ~S ~S" event args)))))
    (:menu-bool (warn "not processed event ~S ~S" event args))
    (:menu-radio (warn "not processed event ~S ~S" event args))
    ((:motion :motion-enter) ; we use simple case with one window so we ignore the window argument
     (destructuring-bind ((x y)) args
       (warn "motioning ~S ~S" x y)))
    (:motion-leave)
    (:focus-enter)
    (:focus-leave)
    (:pressed
     (destructuring-bind ((button x y)) args))
    (:released
     (destructuring-bind ((button x y)) args))
    (:scroll)
    (:resize
     (destructuring-bind ((w h)) args
       (gui-window:window-resize w h lisp-window)))
    (:key-pressed
     (destructuring-bind ((entered key-name key-code mods)) args
       (format t "~&>>> key pressed ~S~%" (list entered key-name key-code mods))
       (cond
         ((equalp entered "a")
          (break "exammine ~S" gui-window:*lisp-app*))
         ((equalp entered "r")
          (restart-helper lisp-window))
         ((equalp entered "p")
          (start-playing-helper lisp-window))
         ((equalp entered "t")
          (show-tutorial-helper))
         (t (warn "undandled key press ~S" args)))))
    (otherwise
     (warn "not handled event ~S ~S" event args)))

  (maphash (lambda (key lwin)
             (declare (ignore key))
             (gui-window:redraw-canvas lwin))
           (gui-window:all-windows)))

;; (defmethod process-event (lisp-window event &rest args)
;;   (warn "@@@ handling weird event @@@ ~S" (list (type-of lisp-window) lisp-window event args) ))

;;; --- enent helpers ----------------------------------------------------------


;;; === main ===================================================================

(defun main ()
  (setf gui-window:*client-fn-draw-objects*  'pong::draw-window
        gui-events:*client-fn-process-event* 'pong::process-event
        gui-window:*client-fn-menu-bar*      'pong::menu-bar
        gui-window:*initial-window-width*    600
        gui-window:*initial-window-height*   400
        gui-window:*timeout-period*          (/ 1000 100)
        gui-window:*initial-title*           "Pong")

  (gui-window:window (make-instance 'pong-window)))

;; (main)
