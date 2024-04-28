(in-package :clops-gui)

(in-package :clops)

;;; =================================================================================
(defun main ()
  ;; (init-model)
  (gui-window:window))


;;; !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

(cffi:defcstruct gdk-rgba
  (red   :float)
  (green :float)
  (blue  :float)
  (alpha :float))

(defun color-to-rgba (color)
  (cffi:with-foreign-object (rgba '(:struct gdk-rgba))
    (let ((pointer (make-instance 'gir::struct-instance
                                  :class (gir:nget gdk::*ns* "RGBA")
                                  :this rgba)))
      (let ((valid-color (gdk:rgba-parse pointer color)))
        (cffi:with-foreign-slots ((red green blue alpha) rgba (:struct gdk-rgba))
          (list valid-color red green blue alpha))))))

(defun set-rgba (color)
  (let ((parsed-color (color-to-rgba color)))
    (if (first parsed-color)
        (apply 'cairo:set-source-rgba (rest parsed-color))
        (error "~S is not a valid color" color))))
;;; ============================ view ==========================================
(defun draw-objects (window)            ; view
  (assert (typep window 'lisp-window))

  (loop for box in (children window)
        do (render box window))

  ;; circle at mouse coordinates
  (when (and (~> window mouse-coordinates)
             ;; now only in testing
             (eq *testing-current-window*
                 (gir-window window)))
    (cairo:set-source-rgba 1 1 0.2 0.4)
    (cairo:set-line-width 6.0)
    (cairo:arc (car (~> window mouse-coordinates))
               (cdr (~> window mouse-coordinates))
               10.0 0 (* 2.0 pi))
    (cairo:fill-path)
    (cairo:arc (car (~> window mouse-coordinates))
               (cdr (~> window mouse-coordinates))
               10.0 0 (* 2.0 pi))
    (set-rgba "green")
    (cairo:set-line-width 0.5)
    (cairo:stroke)))

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
