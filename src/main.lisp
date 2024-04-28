(in-package :clops-gui)

(in-package :clops)

;;; =================================================================================
(defun main ()
  ;; (init-model)
  (gui-window:window))

;;; ================================= All GUI Events ===========================

;;; these functions are for REPL convenience to play with handling the events
(defun de-menu-simple  (action-name)           (process-event :menu-simple action-name))
(defun de-menu-bool    (action-name bool01)    (process-event :menu-bool   action-name bool01))
(defun de-menu-radio   (action-name radiostr)  (process-event :menu-radio  action-name radiostr))
(defun de-motion       (x y window)            (process-event :motion       x y window))
(defun de-motion-enter (x y window)            (process-event :motion-enter x y window))
(defun de-focus-enter  (window)                (process-event :focus-enter  window))
(defun de-motion-leave (window)                (process-event :motion-leave window))
(defun de-focus-leave  (window)                (process-event :focus-leave  window))
(defun de-pressed      (button x y)            (process-event :pressed   button x y))
(defun de-released     (button x y)            (process-event :released  button x y))
(defun de-scroll       (dx dy)                 (process-event :scroll    dx dy))
(defun de-key-pressed  (letter name code mods) (process-event :key-pressed   letter name code mods))
(defun de-key-released (letter name code mods) (process-event :key-released  letter name code mods))
(defun de-resize       (width height window)   (process-event :resize width height window))
(defun de-timeout      ()                      (process-event :timeout))

;;; ======================= update and view ====================================

(let ((motion-count 0))
  (defun process-event (event &rest args)
    (let ((ignored-tracking-event (or (eq event :timeout)
                                      (and (eq event :motion)
                                           (null *track-mouse-motion*)))))

      (if (eq event :motion)
          (incf motion-count)
          (setf motion-count 0))

      (unless ignored-tracking-event
        (format t "~&processing event ~S ~S~%" event args))

      ;; (loop for awp being the hash-key in (windows *lisp-app*)
      ;;       for n = 0 then (1+ n)
      ;;       for awp-lisp-window = (gethash awp (windows *lisp-app*))
      ;;       do (format t "~A zzzzzzzzzz window ~A ~A~%" n awp (type-of awp)))

      ;; active window
      (let* ((appwindow
               (if (slot-boundp *lisp-app* 'gtk4-app)
                   (let ((gaaw (gtk4:application-active-window (gtk4-app *lisp-app*)) ))
                     (when gaaw
                       (gir::this-of gaaw)))
                   *testing-current-window*)))
        (unless (and appwindow
                     (current-window *lisp-app*)
                     (window-current-p *lisp-app* appwindow))
          (format t "~&======== different ====================================~%")
          (setf (current-window *lisp-app*) appwindow)))

      ;; like in Elm runtime calling the update

      (case event
        ;; GUI
        (:menu-simple (cond
                        ((string= (first args) "about")
                         (gui-window:present-about-dialog))
                        ((string= (first args) "new-window")
                         (window-add *lisp-app* (gui-window:new-window-for-app (gtk4-app *lisp-app*))))
                        ((string= (first args) "quit")
                         (gui-window:close-all-windows-and-quit))))
        (:menu-bool )
        (:menu-radio  )
        (:motion
         (apply #'window-motion *lisp-app* args))
        (:focus-enter)
        (:motion-enter
         (apply #'window-hover *lisp-app* args))
        (:focus-leave)
        (:motion-leave
         (window-unhover *lisp-app* (first args)))
        (:pressed )
        (:released )
        (:scroll )
        (:key-pressed (cond ((equal "Escape" (second args))
                             (setf *break-on-key* (not *break-on-key*))
                             (warn "pressed Escape ~S" *break-on-key*))
                            ((equal "F2" (second args))
                             (build-box-class (current-lisp-window *lisp-app*) 'box-section 0 0 50 50))
                            ((equal "F3" (second args))
                             (build-box-class (current-lisp-window *lisp-app*) 'box-section 50 50 250 250))

                            ((equal "F5" (second args))
                             ;; problems start with switch to hashes
                             (cond-step *testing-current-window*
                                        (let ((parent (~> (current-lisp-window *lisp-app*)
                                                          children
                                                          first
                                                          children
                                                          (serapeum/bundle:hash-table-plist _)
                                                          (nth 1 _))))
                                          (clops:build-box-class parent 'box-div 5 5 20 20))))

                            ((equal "F7" (second args))
                             (let ((parent (~> (current-lisp-window *lisp-app*) children first)))
                               (move-right parent)))
                            ((equal "F8" (second args))
                             (let ((parent (~> (current-lisp-window *lisp-app*) children first)))
                               (move-left parent)))

                            (T
                             (warn "unhandled key press ~A" args))))
        (:key-released)
        (:resize
         (window-resize *lisp-app* (first args) (second args) (third args)))
        (:timeout))

      ;; inspired by Gtk4, think of separate from update layout phase
      ;; https://docs.gtk.org/gtk4/drawing-model.html
      ;; geometry changes calculated

      ;; like in Elm runtime calling the view
      (if T
          (loop for awp being the hash-key in (windows *lisp-app*)
                for awp-lisp-window = (gethash awp (windows *lisp-app*))
                for awp-gir-window = (gir-window awp-lisp-window)
                do
                   (if *testing-current-window*
                       ;; paint drawn windows to png file
                       (when nil
                         ;; atm we have nil because we call it in experiment, not in the event handling
                         (simulate-draw-func awp-lisp-window))


                       ;; canvas for awp that later will be passed and drawn as lisp-window
                       (gtk4:widget-queue-draw
                        (~> awp-gir-window gtk4:widget-first-child gtk4:widget-first-child))
                       ))))))

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
;;; !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

;;; both %draw-func and simulate-draw-func create context on each call
;;; we need to investigate whether it is necessary only on the dimensions change

;;; drawing callback ===========================================================
(cffi:defcallback %draw-func :void ((area :pointer)
                                    (cr :pointer)
                                    (width :int)
                                    (height :int)
                                    (data :pointer))
  (declare (ignore data))

  ;; #######################################################################
  (setf cairo:*context* (make-instance 'cairo:context
                                       :pointer cr
                                       :width width
                                       :height height
                                       :pixel-based-p nil))
  ;; call actual drawing
  (draw-objects (window-get *lisp-app* (gtk4:widget-parent
                                        (gtk4:widget-parent
                                         (gir:build-object-ptr (gir:nget gtk4:*ns* "DrawingArea") area)))))
  ;; ###########################################################################

  ;; gtk will put the drawn surface on canvas
  )

(defun simulate-draw-func (window)
  (let* ((surface (cairo:create-image-surface :argb32
                                              (car (dimensions window))
                                              (cdr (dimensions window)))))

    ;; #######################################################################
    (setf  cairo:*context* (cairo:create-context surface))
    ;; call actual drawing
    (draw-objects window)
    ;; #######################################################################

    ;; put drawn surface to a file
    (cairo:surface-write-to-png surface
                                (format nil "~Acairo-simulate-drawing~A-~A.png"
                                        (uiop:temporary-directory)
                                        (get-internal-run-time)
                                        (gir-window window)))))

(defun simulate-draw-func-key (testkey)
  (simulate-draw-func (window-get *lisp-app* testkey)))

;;; ============================ view ==========================================
(defun draw-objects (window)            ; view
    (assert (eq (type-of window) 'lisp-window))

  (let ((cnt (format nil "~A ~S"
                     "Hello, please add some code"
                     :nothing))
        (tpx 0)
        (tpy 0))
    (cairo:set-source-rgb  1 1 1)
    (cairo:paint)


    (cairo:select-font-face "Ubuntu Mono" :normal :bold)
    (cairo:set-font-size 20)
    (multiple-value-bind  (xb yb width height)
        (cairo:text-extents cnt)
      (declare (ignore xb yb width height))
      )
    (setf tpx 6
          tpy 15)


    (cairo:set-source-rgb 0 0 0)
    (cairo:move-to tpx tpy)
    (cairo:show-text (format nil "~A" cnt))

    (cairo:move-to tpx (+ tpy 20))

    (when window
      (cairo:show-text (format nil "~A" (window-stats window)))))

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
