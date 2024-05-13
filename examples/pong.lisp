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

;;; === defgenerics ============================================================

;;; === classes ================================================================
(defclass/std pong-window (gui-window:lisp-window)
  (()))

(defclass/std tutorial-window (gui-window:lisp-window)
  (()))

(defclass/std player ()
  (()))

(defclass/std player-human (player)
  (()))

(defclass/std player-computer (player)
  (()))

(defclass/std slider ()
  (()))

(defclass/std slider-human (slider)
  (()))

(defclass/std slider-computer (slider)
  (()))

(defclass/std ball ()
  (()))

(defclass/std game-area ()
  ((top-left)
   (bottom-right)))

(defclass/std coordinates ()
  ((x)
   (y)))

(defclass/std pong-game ()
  ((player-human    :a :std (make-instance 'player-human))
   (player-computer :a :std (make-instance 'player-computer))
   (ball            :a :std (make-instance 'ball))
   (game-area       :a :std (make-instance 'game-area))
   (pong-window)))

;;; === methods ================================================================
(defmethod start-game ((game pong-game))
  (warn "starting pong"))

(defmethod resize ((game pong-game) w h)
  (warn "resizing pong ~S" game))

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

(defmethod draw-window ((window pong-window))
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

(defmethod draw-window ((window tutorial-window))
  (cairo:set-source-rgb  1 1 1)
  (cairo:paint)

  (cairo:select-font-face "Ubuntu Mono" :normal :bold)
  (cairo:set-font-size 20)

  (let ((my-text "Keyboard Shortcuts"))
    (multiple-value-bind  (xb yb width height)
        (cairo:text-extents my-text)
      (declare (ignore xb yb width height)))
    (cairo:set-source-rgb 0 0 0)
    (cairo:move-to 20 20)
    (cairo:show-text (format nil "~A" my-text)))

  (cairo:select-font-face "Ubuntu Mono" :normal :bold)
  (cairo:set-font-size 10)

  (loop for line in (list "r - Restart"
                              "t - show Tutorial")
        for ln = 0 then (1+ ln)
        do

           (let ((line-text line))
             (multiple-value-bind  (xb yb width height)
                 (cairo:text-extents line-text)
               (declare (ignore xb yb width height)))
             (cairo:set-source-rgb 0 0 0)
             (cairo:move-to 20 (+ 50 (* ln 20)))
             (cairo:show-text (format nil "~A" line-text)))))

;;; === events =================================================================

(defmethod process-event ((lisp-window pong-window) event &rest args)
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
            :website "https://github.com/bigos"
            :program-name "Pong"
            :comments "Nothing to say yet"
            :license "Public Domain"
            :system-information (format nil "~A" (uiop/os:implementation-identifier))
            :logo-icon-name "application-x-addon")))

         (T
          (warn "not processed event ~S ~S" event args)))))
    (:menu-bool (warn "not processed event ~S ~S" event args))
    (:menu-radio (warn "not processed event ~S ~S" event args))
    ((:motion :motion-enter) ; we use simple case with one window so we ignore the window argument
     (destructuring-bind ((x y)) args
       (setf (gui-window:mouse-coordinates gui-window:*lisp-app*) (cons x y)
             (gui-window:current-motion    gui-window:*lisp-app*) lisp-window)))
    (:motion-leave
     (setf (gui-window:mouse-coordinates gui-window:*lisp-app*) nil
           (gui-window:current-motion gui-window:*lisp-app*) nil))
    (:focus-enter)
    (:focus-leave)
    (:pressed)
    (:released)
    (:scroll)
    (:resize
     (destructuring-bind ((w h)) args
       (gui-window:window-resize w h lisp-window)
       (when *pong-game* (resize *pong-game* w h))))
    (:key-pressed
     (destructuring-bind ((entered key-name key-code mods)) args
       (format t "~&>>> key pressed ~S~%" (list entered key-name key-code mods))
       (cond

         ((equalp entered "r")
          (restart-helper lisp-window))
         ((equalp entered "t")
          (show-tutorial-helper))
         (t (warn "undandled key press ~S" args)))))
    (otherwise
     (warn "not handled event ~S ~S" event args)))

  (maphash (lambda (key lwin)
             (declare (ignore key))
             (gui-window:redraw-canvas lwin))
           (gui-window:all-windows)))

(defmethod process-event ((lisp-window tutorial-window) event &rest args)
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
    ((:motion :motion-enter) ; we use simple case with one window so we ignore the window argument
     (destructuring-bind ((x y)) args
       (setf (gui-window:mouse-coordinates gui-window:*lisp-app*) (cons x y)
             (gui-window:current-motion    gui-window:*lisp-app*) lisp-window)))
    (:motion-leave
     (setf (gui-window:mouse-coordinates gui-window:*lisp-app*) nil
           (gui-window:current-motion gui-window:*lisp-app*) nil))
    (:focus-enter)
    (:focus-leave)
    (:pressed)
    (:released)
    (:scroll)
    (:resize)
    (:key-pressed
     (destructuring-bind ((entered key-name key-code mods)) args
       (format t "~&>>> key pressed ~S~%" (list entered key-name key-code mods))))
    (otherwise
     (warn "not handled event ~S ~S" event args))))

;; (defmethod process-event (lisp-window event &rest args)
;;   (warn "@@@ handling weird event @@@ ~S" (list (type-of lisp-window) lisp-window event args) ))

;;; --- enent helpers ----------------------------------------------------------
(defmethod restart-helper ((pong-window pong-window))
  (setf *pong-game* (make-instance 'pong-game :pong-window pong-window)))

(defun show-tutorial-helper ()
  (gui-window:window-creation-from-menu
   "Tutorial"
   nil                                  ; no menu
   (make-instance 'tutorial-window)))

;;; === main ===================================================================

(defun main ()
  (setf gui-window:*client-fn-menu-bar*      'pong::menu-bar
        gui-window:*client-fn-draw-objects*  'pong::draw-window
        gui-events:*client-fn-process-event* 'pong::process-event
        gui-window:*initial-window-width*    600
        gui-window:*initial-window-height*   400
        gui-window:*initial-title*           "Pong")

  (gui-window:window (make-instance 'pong-window)))

;; (main)
