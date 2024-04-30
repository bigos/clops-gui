;; (load "~/Programming/Lisp/clops-gui/examples/example.lisp")

(in-package #:cl)
;;; === load ===================================================================
(push #p "~/Programming/Lisp/clops-gui/" ql:*local-project-directories*)
(ql:quickload '(:clops-gui :serapeum))

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

  (let ((my-text "Try moving the mouse over the window."))
    (multiple-value-bind  (xb yb width height)
        (cairo:text-extents my-text)
      (declare (ignore xb yb width height)))

    (cairo:set-source-rgb 0 0 0)
    (cairo:move-to 10 20)
    (cairo:show-text (format nil "~A" my-text)))
  (cairo:move-to 10 50)
  (cairo:show-text (format nil "~A" (mouse-coordinates *model*))))

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
(defun process-event (event args)
  (case event
    (:timeout
     nil)
    ((:motion :motion-enter) ; we use simple case with one window so we ignore the window argument
     (setf (mouse-coordinates *model*)
           (cons (first args) (second args)))
     (setf (gtk4:widget-sensitive-p (third args)) t)
     (warn "zzzzzzzzzzzzzzz ~A ~A ~A" event args (mouse-coordinates *model*)))
    (:motion-leave
     (setf (mouse-coordinates *model*)
           nil))
    (:key-pressed
     (when (equal (first args) "k")
       (break "mouse coordinates ~S" (mouse-coordinates *model*))))
    (otherwise
     (warn "not processed event ~S ~S" event args)))

  (loop for awp being the hash-key in (gui-window:windows gui-window:*lisp-app*)
        for awp-lisp-window = (gethash awp (gui-window:windows gui-window:*lisp-app*))
        for awp-gir-window = (gui-window:gir-window awp-lisp-window)
        for wn = 0 then (1+ wn)
        do
           (progn
             (when nil
               ;; atm we have nil because we call it in experiment, not in the event handling
               (gui-window:simulate-draw-func awp-lisp-window))

             ;; canvas for awp that later will be passed and drawn as lisp-window
             (gtk4:widget-queue-draw
              (serapeum:~> awp-gir-window gtk4:widget-first-child gtk4:widget-first-child )))))

(defun init ()
  ;; define external functions
  (setf
   gui-window:*draw-objects-fn*  'cl::draw-objects
   gui-window:*menu-bar-menu-fn* 'cl::menu-bar-menu
   gui-events:*process-event-fn* 'cl::process-event)
  (setf *model* (make-instance 'model)))

(defparameter *model* nil)

(defun main ()
  (init)
  (gui-window:window))

(main)
