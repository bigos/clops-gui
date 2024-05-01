;; (load "~/Programming/Lisp/clops-gui/examples/example.lisp")
(declaim (optimize (speed 0) (safety 2) (debug 3)))

(in-package #:cl)
;;; === load ===================================================================
(push #p "~/Programming/Lisp/clops-gui/" ql:*local-project-directories*)
(ql:quickload '(:clops-gui))

;;; === classes ================================================================
(defclass-std:defclass/std model ()
  ((mouse-coordinates :std nil)))

;;; ============================ view ==========================================
(defun draw-objects (window)            ; view
  (assert (typep window 'gui-window:lisp-window))
  (warn "would draw on window")

  (cairo:set-source-rgb  1 1 1)
  (cairo:paint)

  (cairo:select-font-face "Ubuntu Mono" :normal :bold)
  (cairo:set-font-size 20)

  (let ((my-text "Try moving the mouse over the window or resizing it."))
    (multiple-value-bind  (xb yb width height)
        (cairo:text-extents my-text)
      (declare (ignore xb yb width height)))
    (cairo:set-source-rgb 0 0 0)
    (cairo:move-to 10 20)
    (cairo:show-text (format nil "~A" my-text)))

  (if (null (mouse-coordinates *model*))
      (gui-window:set-rgba "red")
      (gui-window:set-rgba "#002244AA"))
  (cairo:move-to 10 50)
  (cairo:show-text (format nil "~A" (mouse-coordinates *model*)))

  (if (< (car (gui-window:dimensions window)) 200)
      (gui-window:set-rgba "red")
      (gui-window:set-rgba "green"))
  (cairo:move-to 10 80)
  (cairo:show-text (format nil "~A" (gui-window::dimensions window)))

  )

;;; ===================== menu declaration =====================================

(defun menu-bar-menu (app)
  (let ((menu (gio:make-menu)))
    (gui-menu:build-menu
     menu
     (gui-menu:prepare-submenu
      "File"
      ;; (prepare-section
      ;;  nil
      ;;  (build-items
      ;;   (prepare-item-bool app menu "Dark mode" "dark-mode" nil)))

      (gui-menu:prepare-section
       nil
       (gui-menu:build-items
        (gui-menu:prepare-item-simple app menu "New Window" "new-window")))

      ;; (prepare-submenu
      ;;  "New Game"
      ;;  (prepare-section
      ;;   nil
      ;;   (progn
      ;;     (prepare-radio-action app "new-game-size" "SMALL")
      ;;     (build-items
      ;;      (prepare-item-radio app menu "Small 8x8"    "new-game-size" "SMALL")
      ;;      (prepare-item-radio app menu "Medium 16x16" "new-game-size" "MEDIUM")
      ;;      (prepare-item-radio app menu "Large 32x32"  "new-game-size" "LARGE")))))

      (gui-menu:prepare-section
       nil
       (gui-menu:build-items
        (gui-menu:prepare-item-simple app menu "Quit" "quit"))))
     (gui-menu:prepare-submenu
      "Help"
      ;; for now I plan to have only the About menu item
      ;; (prepare-section
      ;;  nil
      ;;  (build-items
      ;;   (prepare-item-simple app menu "Help" "help")
      ;;   (prepare-item-simple app menu "Tutorial" "tutorial" :disabled T)))
      (gui-menu:prepare-section
       nil
       (gui-menu:build-items
        (gui-menu:prepare-item-simple app menu "About" "about")))))

    (values menu)))

;;; ====================== event processing ====================================
(defun process-event (event &rest args)
  (case event
    (:timeout
     nil)
    (:resize
     (destructuring-bind ((w h win)) args
       (gui-window::window-resize w h win)))
    ((:motion :motion-enter) ; we use simple case with one window so we ignore the window argument
     (setf (mouse-coordinates *model*)
           (cons (first args) (second args))))
    (:motion-leave
     (setf (mouse-coordinates *model*)
           nil))
    (:key-pressed
     (when (equal (first args) "k")
       (break "mouse coordinates ~S" (mouse-coordinates *model*))))
    (otherwise
     (warn "not processed event ~S ~S" event args)))

  (maphash (lambda (key lwin) (gui-window:redraw-canvas lwin))
           (gui-window:all-windows)))

;;; REPL usege (cl::experiment)
(defun experiment ()
  (setf
   *model* (make-instance 'model)
   gui-window:*lisp-app* (make-instance 'gui-window::lisp-app))
  (assert (zerop (hash-table-count (gui-window:all-windows))))
  (gui-window::window-add gui-window:*lisp-app* :testing)
  (process-event :resize (list 600 200 (gui-window:window-symb :testing)))
  (process-event :timeout)
  (process-event :motion-enter (list 50 50 (gui-window:window-symb :testing)))
  ;; end
  (warn "please check your folder ~S for images drawn by the procedure simulate-draw-func"
        (uiop:temporary-directory)))

(defun init ()
  ;; define external functions
  (setf
   gui-window:*draw-objects-fn*  'cl::draw-objects
   gui-window:*menu-bar-menu-fn* 'cl::menu-bar-menu
   gui-events:*process-event-fn* 'cl::process-event
   gui-window:*initial-title* "Example window"
   )
  (setf *model* (make-instance 'model)))

(defparameter *model* nil)

(defun main ()
  (init)
  (gui-window:window))

(main)
