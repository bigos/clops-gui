;; (load "~/Programming/Lisp/clops-gui/example.lisp")

(in-package #:cl)

(push #p "~/Programming/Lisp/clops-gui/" ql:*local-project-directories*)
(ql:quickload :clops-gui)

;;; ============================ view ==========================================
(defun draw-objects (window)            ; view
    (assert (typep window 'gui-window::lisp-window))
  (warn "would draw on window"))

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
  (unless (member event '(:timeout :motion))
    (warn "prcessing event ~S ~S" event args)))

(defun init ()
  (warn "starting init")
  (setf
   gui-window::*draw-objects-fn* 'cl::draw-objects
   gui-window::*menu-bar-menu-fn* 'cl::menu-bar-menu
   gui-events::*process-event-fn* 'cl::process-event
   ))

(defun main ()
  (init)
  (gui-window:window))

(main)
