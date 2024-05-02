;; (load "~/Programming/Lisp/clops-gui/examples/example.lisp")
(declaim (optimize (speed 0) (safety 2) (debug 3)))

(in-package #:cl)
;;; === load ===================================================================
(push #p "~/Programming/Lisp/clops-gui/" ql:*local-project-directories*)
(ql:quickload '(:clops-gui) :silent t)

;;; === classes ================================================================
(defclass-std:defclass/std model ()
  ((current-motion)
   (current-focus)
   (mouse-coordinates :std nil)))

;;; ====== methods =============================================================
(defun currents (model)
  (warn "current arhs ~S" (list (current-motion model)
                                (current-focus model)))
  (let ((current-motion (when (current-motion model)
                          (gui-window:hasher (current-motion model))))
        (current-focus  (when (current-focus model)
                          (gui-window:hasher (current-focus model)))))
    (warn "passed the 2 vars ~A ~a" current-motion current-focus)
    (warn "currents ~S" (list current-motion
                              current-focus
                              (type-of current-motion)
                              (type-of current-focus)))))

;;; ============================= experimental testing =========================
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

;;; ============================= client functions =============================
;;; ============================ view ==========================================
(defun draw-objects (window)            ; view
  (assert (typep window 'gui-window:lisp-window))
  ;; (warn "would draw on window")

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
  (cairo:show-text (format nil "~A" (gui-window:dimensions window)))

  )

;;; ===================== menu declaration =====================================

(defun menu-bar (app)
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
  (format t "~&going to process ~A ~A  " event args)
  (case event
    (:timeout
     nil)
    (:menu-simple
     (destructuring-bind ((menu-item)) args
       (warn "menu item ~s" menu-item)
       (cond ((equal menu-item "new-window")
              (gui-window:window-activation-from-menu (format nil
                                                              "~A ~A"
                                                              gui-window:*initial-title*
                                                              (get-internal-run-time))))
             ((equal menu-item "quit")
              (gui-window:close-all-windows-and-quit))

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
     (destructuring-bind ((x y win)) args
       (setf (mouse-coordinates *model*) (cons x y)
             (current-motion    *model*) win))
     (currents *model*))
    (:motion-leave
     (setf (mouse-coordinates *model*) nil
           (current-motion *model*) nil)
     (currents *model*))
    (:focus-enter
     (destructuring-bind ((win)) args
       (setf (current-focus *model*) win))
     (currents *model*))
    (:focus-leave
     (setf (current-focus *model*) nil)
     (currents *model*))
    (:pressed (warn "not processed event ~S ~S" event args))
    (:released (warn "not processed event ~S ~S" event args))
    (:scroll (warn "not processed event ~S ~S" event args))
    (:resize
     (destructuring-bind ((w h win)) args
       (gui-window:window-resize w h win)))
    (:key-pressed
     (currents cl::*model*)
     (when (equal (first args) "k")
       (break "mouse coordinates ~S" (mouse-coordinates *model*))))
    (otherwise
     (warn "not processed event ~S ~S" event args)))

  (maphash (lambda (key lwin) (gui-window:redraw-canvas lwin))
           (gui-window:all-windows)))

(defun init ()
  ;; define external functions
  (setf
   gui-window:*client-fn-draw-objects*  'cl::draw-objects
   gui-window:*client-fn-menu-bar*      'cl::menu-bar
   gui-events:*client-fn-process-event* 'cl::process-event
   gui-window:*initial-title*           "Example window")

  (setf *model* (make-instance 'model)))

(defparameter *model* nil)

(defun main ()
  (init)
  (gui-window:window))

(main)
