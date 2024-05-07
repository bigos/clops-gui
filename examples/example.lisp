;; (load "~/Programming/Lisp/clops-gui/examples/example.lisp")
(declaim (optimize (speed 0) (safety 2) (debug 3)))

(defpackage #:example
  (:use #:cl))

(in-package #:example)
;;; === load ===================================================================
(push #p "~/Programming/Lisp/clops-gui/" ql:*local-project-directories*)
(ql:quickload '(:clops-gui) :silent nil)

;;; === classes ================================================================
(defclass-std:defclass/std model ()
  ((timeout-count :std 0)))

(defclass-std:defclass/std app-window (gui-window:lisp-window)
  ())

(defclass-std:defclass/std help-window (gui-window:lisp-window)
  ())

;;; ====== methods =============================================================
(defmethod inc-timer ((model model))
    (incf (timeout-count *model*))
  (when (> (timeout-count *model*) 5)
    (setf (timeout-count *model*) 0)))

;;; ============================= experimental testing =========================
;;; REPL usage (cl::experiment)
(defun experiment ()
  (setf
   *model* (make-instance 'model)
   gui-window:*lisp-app* (make-instance 'gui-window::lisp-app))
  (assert (zerop (hash-table-count (gui-window:all-windows))))
  (gui-window::window-creation-from-simulation :testing)
  (assert (eq 1 (hash-table-count (gui-window:all-windows))))
  (process-event :resize (list 600 200 (gui-window:window-get gui-window:*lisp-app* :testing)))
  (process-event :timeout)
  (process-event :motion-enter (list 50 50 (gui-window:window-get gui-window:*lisp-app* :testing)))

  (warn "trying tools")
  (assert (eq 1 (hash-table-count (gui-window:all-windows))))
  (gui-window::window-creation-from-simulation :tools)
  (warn "should have tools window")
  (assert (eq 2 (hash-table-count (gui-window:all-windows))))
  ;; seems like resize is changing the wrong window
  (warn "added window trying to resize")
  (process-event :resize (list 250 500 (gui-window:window-get gui-window:*lisp-app* :tools)))
  (warn "resized tools")
  (process-event :timeout)
  (process-event :motion-enter (list 10 10 (gui-window:window-get gui-window:*lisp-app* :tools)))

  ;; end
  (warn "please check your folder ~S for images drawn by the procedure simulate-draw-func"
        (uiop:temporary-directory)))

;;; ============================= client functions and methods =================
;;; ===================== menu declaration =====================================

(defun menu-bar-simple (app lisp-window)
  (let ((menu (gio:make-menu)))
    (gui-menu:build-menu
     menu
     (gui-menu:prepare-submenu
      "File"
      (gui-menu:prepare-section
       nil
       (gui-menu:build-items
        (gui-menu:prepare-item-simple lisp-window app menu "Quit" "quit")))))

    (values menu)))

(defun menu-bar (app lisp-window)
    (let ((menu (gio:make-menu)))
      (gui-menu:build-menu
       menu
       (gui-menu:prepare-submenu
        "File"
        (gui-menu:prepare-section
         nil
         (gui-menu:build-items
          (gui-menu:prepare-item-bool lisp-window app menu "Dark mode" "dark-mode" nil)))

        (gui-menu:prepare-section
         nil
         (gui-menu:build-items
          (gui-menu:prepare-item-simple lisp-window app menu "New Window" "new-window")))

        (gui-menu:prepare-submenu
         "New Game"
         (gui-menu:prepare-section
          nil
          (progn
            (gui-menu:prepare-radio-action lisp-window app "new-game-size" "SMALL")
            (gui-menu:build-items
             (gui-menu:prepare-item-radio lisp-window app menu "Small 8x8"    "new-game-size" "SMALL")
             (gui-menu:prepare-item-radio lisp-window app menu "Medium 16x16" "new-game-size" "MEDIUM")
             (gui-menu:prepare-item-radio lisp-window app menu "Large 32x32"  "new-game-size" "LARGE")))))

        (gui-menu:prepare-section
         nil
         (gui-menu:build-items
          (gui-menu:prepare-item-simple lisp-window app menu "Quit" "quit"))))
       (gui-menu:prepare-submenu
        "Help"
        ;; for now I plan to have only the About menu item
        (gui-menu:prepare-section
         nil
         (gui-menu:build-items
          (gui-menu:prepare-item-simple lisp-window app menu "Help" "help")
          (gui-menu:prepare-item-simple lisp-window app menu "Tutorial" "tutorial" :disabled T)))
        (gui-menu:prepare-section
         nil
         (gui-menu:build-items
          (gui-menu:prepare-item-simple lisp-window app menu "About" "about")))))

      (values menu)))

;;; ============================ view ==========================================
(defmethod draw-window ((window gui-window:lisp-window))            ; view
  (assert (typep window 'gui-window:lisp-window))
  ;; (warn "would draw on window")

  (cairo:set-source-rgb  1 1 1)
  (cairo:paint)

  (cairo:select-font-face "Ubuntu Mono" :normal :bold)
  (cairo:set-font-size 10)

  (let ((my-text "Try moving the mouse over the window or resizing it."))
    (multiple-value-bind  (xb yb width height)
        (cairo:text-extents my-text)
      (declare (ignore xb yb width height)))
    (cairo:set-source-rgb 0 0 0)
    (cairo:move-to 10 20)
    (cairo:show-text (format nil "~A" my-text)))

  (cairo:select-font-face "Ubuntu Mono" :normal :bold)
  (cairo:set-font-size 15)

  (if (null (gui-window:mouse-coordinates gui-window:*lisp-app*))
      (gui-window:set-rgba "red")
      (gui-window:set-rgba "#002244AA"))
  (cairo:move-to 10 50)
  (when (gui-window:current-motion-window gui-window:*lisp-app* window)
    (cairo:show-text (format nil "~A" (gui-window:mouse-coordinates gui-window:*lisp-app*))))

  (cairo:select-font-face "Ubuntu Mono" :normal :normal)

  (if (< (car (gui-window:dimensions window)) 200)
      (gui-window:set-rgba "red")
      (gui-window:set-rgba "green"))
  (cairo:move-to 10 80)
  (cairo:show-text (format nil "~A" (gui-window:dimensions window)))

  (cairo:select-font-face "Ubuntu Mono" :italic :bold)

  (cairo:move-to 10 100)
  (let ((cmotion    (gui-window:current-motion-window gui-window:*lisp-app* window)))
    (if cmotion
        (gui-window:set-rgba "green")
        (gui-window:set-rgba "red"))
    (cairo:show-text (format nil "motion ~A" cmotion)))

  (cairo:select-font-face "Ubuntu Mono" :oblique :normal)

  (cairo:move-to 10 120)
  (let ((cfocus (gui-window:window-hkey (gui-window:current-focus-window gui-window:*lisp-app* window))))
    (if cfocus
        (gui-window:set-rgba "green")
        (gui-window:set-rgba "red"))
    (cairo:show-text (format nil "focus ~A"
                             cfocus)))

  (cairo:move-to 10 140)
  (gui-window:set-rgba "blue")
  (cairo:show-text (format nil "window-hkey ~A" (gui-window:window-hkey window)))

  (cairo:move-to 10 160)
  (gui-window:set-rgba "black")
  (cairo:show-text (format nil "count ~A" (timeout-count *model*)))

  )

(defmethod draw-window ((window help-window))
  (cairo:set-source-rgb  1 1 1)
  (cairo:paint)

  (cairo:select-font-face "Ubuntu Mono" :normal :bold)
  (cairo:set-font-size 30)

  (let ((my-text "HEEELP ME TO LEARN LISP!!!"))
    (multiple-value-bind  (xb yb width height)
        (cairo:text-extents my-text)
      (declare (ignore xb yb width height)))
    (cairo:set-source-rgb 0 0 0)
    (cairo:move-to 10 20)
    (cairo:show-text (format nil "~A" my-text)))
  )

;;; ====================== event processing ====================================
(defmethod process-event ((lisp-window gui-window:lisp-window) event &rest args)
  (unless (member event '(:timeout :motion))
    (format t "~&going to process ~S ~A ~A  "
            (class-of lisp-window)
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
     (inc-timer *model*))
    (:menu-simple
     (destructuring-bind ((menu-item)) args
       (warn "menu item ~s" menu-item)
       (cond ((equal menu-item "new-window")
              (gui-window:window-creation-from-menu
               (format nil
                       "~A ~A"
                       gui-window:*initial-title*
                       (get-internal-run-time))
               ;; commenting it out will show windows without menu
               'example::menu-bar-simple
               'example::app-window))
             ((equal menu-item "quit")
              (gui-window:close-all-windows-and-quit))

             ((equal menu-item "help")
              (gui-window:window-creation-from-menu
               (format nil "~A" "Help")
               nil
               'example::help-window))

             ((equal menu-item "about")
              (gui-window:present-about-dialog
               (list
                :authors (list "Jacek Podkanski")
                :website "https://github.com/bigos"
                :program-name "Example"
                :comments "Nothing to say"
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
    (:pressed (warn "not processed event ~S ~S" event args))
    (:released (warn "not processed event ~S ~S" event args))
    (:scroll (warn "not processed event ~S ~S" event args))
    (:resize
     (destructuring-bind ((w h)) args
       (gui-window:window-resize w h lisp-window)))
    (:key-pressed
     )
    (otherwise
     (warn "not handled event ~S ~S" event args)))

  (maphash (lambda (key lwin) (gui-window:redraw-canvas lwin))
           (gui-window:all-windows)))

(defun init ()
  ;; define external functions
  (setf
   gui-window:*client-fn-menu-bar*      'example::menu-bar
   gui-window:*client-fn-draw-objects*  'example::draw-window
   gui-events:*client-fn-process-event* 'example::process-event
   gui-window:*initial-title*           "Example window"
   )

  (setf *model* (make-instance 'model)))

(defparameter *model* nil)

(defun main ()
  (init)

  (gui-window:window (make-instance 'gui-window:lisp-window)))

(main)
