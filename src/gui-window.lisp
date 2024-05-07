;;; ============================ package gui-window ============================
(in-package #:gui-window)

(defparameter *client-fn-menu-bar* nil)
(defparameter *client-fn-draw-objects* nil)
(defparameter *initial-window-width* 400)
(defparameter *initial-window-height* 200)
(defparameter *initial-title* "change me")
(defparameter *lisp-app* nil)
(defparameter *window-class* nil)

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

  ;; ###########################################################################
  (setf cairo:*context* (make-instance 'cairo:context
                                       :pointer cr
                                       :width width
                                       :height height
                                       :pixel-based-p nil))
  ;; call actual drawing
  (funcall *client-fn-draw-objects* (window-get *lisp-app* (gtk4:widget-parent
                                                     (gtk4:widget-parent
                                                      (gir:build-object-ptr (gir:nget gtk4:*ns* "DrawingArea") area)))))
  ;; ############################################################################

  ;; gtk will put the drawn surface on canvas
  )

(defun simulate-draw-func (window)
  (let* ((surface (cairo:create-image-surface :argb32
                                              ;; use defaults if dimensions are nil
                                              (or (car (dimensions window)) 150)
                                              (or (cdr (dimensions window)) 100))))

    ;; #########################################################################
    (setf  cairo:*context* (cairo:create-context surface))
    ;; call actual drawing
    (funcall *client-fn-draw-objects* window)
    ;; #########################################################################

    ;; put drawn surface to a file
    (cairo:surface-write-to-png surface
                                (format nil "~Acairo-simulate-drawing~A-~A.png"
                                        (uiop:temporary-directory)
                                        (get-internal-run-time)
                                        (gir-window window)))))

;;; ========================== windows =========================================

(defclass/std lisp-app ()
  ((gtk4-app :type gir::object-instance)
   (windows :std (make-hash-table))
   (current-motion)
   (current-focus) ; it is not reliable for the last created window
   (mouse-coordinates)
   ))

(defclass/std lisp-window ()
  ((gir-window  :type (or gir::object-instance keyword)
                :documentation "Either gir window or symbol used in test drawing")
   (dimensions :documentation "Cons with width and height or resized window")))

;;; ====== all windows =========================================================
(defun all-windows ()
    (windows *lisp-app*))

;;; ========================== window manipulation =============================
(defmethod (setf current-focus) :before ((window T) (lisp-app lisp-app))
  (warn "~&??????????????????? setting current focus to ~s ~s~%~%"
        (window-hkey window)
        (type-of window)))

(defmethod current-motion-window ((lisp-app lisp-app) (window t))
  (let ((m (window-hkey (current-motion lisp-app)))
        (w (window-hkey window)))
    (eq m w)))

(defmethod current-focus-window  ((lisp-app lisp-app) (window t))
  (let ((h (window-hkey (current-focus lisp-app)))
        (w (window-hkey window)))
    (eq h w)))

(defmethod redraw-canvas ((window lisp-window))
  (etypecase (gir-window window)
    (keyword
     (simulate-draw-func (window-get *lisp-app* window)))
    (t
     (gtk4:widget-queue-draw
      (serapeum:~> window gir-window gtk4:widget-first-child gtk4:widget-first-child)))))

(defmethod window-hkey ((window gir::object-instance))
  (cffi:pointer-address (gir::this-of window)))
(defmethod window-hkey ((window symbol))
  "This form is only used in simulated drawing, but not in gtk4"
  window)
(defmethod window-hkey ((window lisp-window))
  (window-hkey
   (gir-window window)))

(defun window-assert (window)
      (assert (or (typep window 'gir::object-instance)
                  (typep window 'sb-sys:system-area-pointer)
                  (typep window 'symbol))))

(defmethod window-get ((app lisp-app) (window T))
  (warn "existing windows ~S" (loop for k being the hash-key of (windows app) collect k ))
  (gethash (window-hkey window)
           (windows app)))

(defmethod window-resize (w h win)
  (setf (dimensions (window-get *lisp-app* win)) (cons w h)))

(defmethod window-remove ((app lisp-app) (window T))
    (window-assert window)
  (if (remhash (window-hkey window) (windows app))
      (warn "success, window removed ~S" window)
      (warn "strange, window not removed ~S" window)))

;; =========================== dialogs =========================================
(defun present-about-dialog (data)
  (let ((dialog (about-dialog data)))
    (setf (gtk4:window-modal-p dialog) t
          (gtk4:window-transient-for dialog) (gtk4:application-active-window (gtk4-app *lisp-app*)))
    (gtk4:window-present dialog)))

(defun about-dialog (data)
  (warn "called about dialog")
  (let ((dialog (gtk4:make-about-dialog)))
    (setf (gtk4:about-dialog-authors      dialog) (getf data :authors)
          (gtk4:about-dialog-website      dialog) (getf data :website)
          (gtk4:about-dialog-program-name dialog) (getf data :program-name)
          (gtk4:about-dialog-comments     dialog) (getf data :comments)
          (gtk4:about-dialog-license      dialog) (getf data :license)
          (gtk4:about-dialog-system-information dialog) (getf data :system-information)
          (gtk4:about-dialog-logo-icon-name dialog) (getf data :logo-icon-name))
    (values dialog)))

;; =========================== closing everything ==============================
(defun close-all-windows-and-quit ()
  (loop for aw = (gtk4:application-active-window (gtk4-app *lisp-app*))
        until (null aw)
        do (gtk4:window-close aw)))

;; ============================== app windows ==================================

(defun app-windows ()
  (when (gtk4-app *lisp-app*)
    (let ((app-windows (gtk4:application-windows (gtk4-app *lisp-app*))))
      (loop for pos from 0 below (glib:glist-length app-windows)
            collect (glib:glist-nth app-windows pos)))))

;; ============================= key event translation =========================
(defun translate-key-args (args)
  (destructuring-bind (keyval keycode keymods) args
    (list
     (format nil "~A"
             (let ((unicode (gdk:keyval-to-unicode keyval)))
               (if (or (zerop unicode)
                       (member keyval
                               (list gdk:+key-escape+
                                     gdk:+key-backspace+
                                     gdk:+key-delete+)))
                   ""
                   (code-char unicode))))
     (gdk:keyval-name keyval)
     keycode
     (remove-if (lambda (m) (member m '(:num-lock)))
                (loop
                      for modname in '(:shift :caps-lock :ctrl :alt
                                       :num-lock :k6 :win :alt-gr)
                      for x = 0 then (1+ x)
                      for modcode = (mask-field (byte 1 x) keymods)
                      unless (zerop modcode)
                      collect modname)))))

;; ============================ events =========================================
(defun window-events (window lisp-window)
  (let ((key-controller   (gtk4:make-event-controller-key))
        (focus-controller (gtk4:make-event-controller-focus)))

    (gtk4:widget-add-controller window key-controller)
    (gtk4:widget-add-controller window focus-controller)

    (gtk4:connect key-controller "key-pressed"
                  (lambda (e &rest args)
                    (declare (ignore e))
                    (setf (current-focus *lisp-app*) window)
                    (apply #'gui-events:de-key-pressed
                           (append (list lisp-window)
                                   (funcall #'translate-key-args args)
                                   (list window)))))

    (gtk4:connect key-controller "key-released"
                  (lambda (e &rest args)
                    (declare (ignore e))
                    (apply #'gui-events:de-key-released
                           (append (list lisp-window)
                                   (funcall #'translate-key-args args)))))
    ;; for some reason enter and leave are not reliable and I need to add window as in key-pressed to some events
    (gtk4:connect focus-controller "enter"
                  (lambda (e &rest args)
                    (declare (ignore e args))
                    (setf (current-focus *lisp-app*) window)
                    (apply #'gui-events:de-focus-enter (list lisp-window))))

    (gtk4:connect focus-controller "leave"
                  (lambda (e &rest args)
                    (declare (ignore e args))
                    (setf (current-focus *lisp-app*) nil)
                    (apply #'gui-events:de-focus-leave (list lisp-window)))))

  (glib:timeout-add 1000
                    (lambda (&rest args)
                      (declare (ignore args))
                      (funcall #'gui-events:de-timeout lisp-window)
                      glib:+source-continue+))

  (gtk4:connect window "close-request" (lambda (widget &rest args)
                                         (declare (ignore widget args))
                                         (window-remove *lisp-app* window)
                                         (gtk4:window-close window))))

(defun canvas-events (canvas lisp-window)
  (labels
      ((canvas-window (c)
         (gtk4:widget-parent (gtk4:widget-parent c))))
    (let ((motion-controller (gtk4:make-event-controller-motion)))
      (gtk4:widget-add-controller canvas motion-controller)

      (gtk4:connect motion-controller "motion"
                    (lambda (e &rest args) (declare (ignore e)) (apply #'gui-events:de-motion
                                                                       (append (list lisp-window)
                                                                               args
                                                                               (list)))))
      (gtk4:connect motion-controller "enter"
                    (lambda (e &rest args) (declare (ignore e)) (apply #'gui-events:de-motion-enter
                                                                       (append (list lisp-window)
                                                                               args))))
      (gtk4:connect motion-controller "leave"
                    (lambda (e &rest args) (declare (ignore e)) (apply #'gui-events:de-motion-leave
                                                                       (append (list lisp-window)
                                                                               args)))))

    (let ((scroll-controller (gtk4:make-event-controller-scroll :flags gtk4:+event-controller-scroll-flags-vertical+)))
      (gtk4:widget-add-controller canvas scroll-controller)
      (gtk4:connect scroll-controller "scroll"
                    (lambda (e &rest args) (declare (ignore e)) (apply #'gui-events:de-scroll
                                                                       (append (list lisp-window))
                                                                       args))))

    (let ((gesture-click-controller (gtk4:make-gesture-click))
          (click-fn (lambda (event args click-de-fn)
                      (let ((current-button (gtk4:gesture-single-current-button event)))
                        (apply click-de-fn (list lisp-window
                                                 current-button
                                                 (nth 1 args)
                                                 (nth 2 args)))))))
      ;; make gesture click listen to other mouse buttons as well
      (setf (gtk4:gesture-single-button gesture-click-controller) 0)
      (gtk4:widget-add-controller canvas gesture-click-controller)

      (gtk4:connect gesture-click-controller "pressed"
                    (lambda (event &rest args) (apply click-fn (list event args #'gui-events:de-pressed))))
      (gtk4:connect gesture-click-controller "released"
                    (lambda (event &rest args) (apply click-fn (list event args #'gui-events:de-released)))))


    ;; for some reason resize signal does not work without  notify
    (gtk4:connect canvas "notify" (lambda (widget &rest args)
                                    (format t "~&>>>>>>>>>>>>>>>>>>>>>>>> notifying ~S ~S~%" widget args)))

    (gtk4:connect canvas "resize" (lambda (widget &rest args)
                                    (declare (ignore widget))
                                    (gui-events:de-resize lisp-window (first args) (second args))))))

;; =============================================================================

(defun new-window-for-app (app window-title window-menu-fn lisp-window)
    (let ((window (gtk4:make-application-window :application app)))
      (gtk4:application-add-window app window)

      (when window-menu-fn
        (setf
         (gtk4:application-menubar app) (funcall window-menu-fn app)
         (gtk4:application-window-show-menubar-p window) T))

      (setf
       (gtk4:window-title window) window-title
       ;; it may not be needed as in wsl we can still use left edge and corners
       ;; (gtk4:window-resizable-p window) T
       (gtk4:window-default-size window) (list *initial-window-width*
                                               *initial-window-height*))

      (let ((box (gtk4:make-box :orientation gtk4:+orientation-vertical+
                                :spacing 0)))
        (let ((canvas (gtk4:make-drawing-area)))

          (setf (gtk4:widget-vexpand-p canvas) T
                (gtk4:drawing-area-draw-func canvas) (list (cffi:callback %draw-func)
                                                           (cffi:null-pointer)
                                                           (cffi:null-pointer)))
          (canvas-events canvas lisp-window)
          (gtk4:box-append box canvas))
        (setf (gtk4:window-child window) box))

      (window-events window lisp-window)

      (format t "actions defined for app ~A~%"  (gio:action-group-list-actions app))

      (gtk:window-present window)

      window))

(defun window-activation-and-connection (lisp-app gtk4-app window-title window-menu-fn window-class)
  (if gtk4-app
      (let* ((new-lisp-window (make-instance window-class))
             (new-gtk4-window (new-window-for-app gtk4-app window-title window-menu-fn new-lisp-window)))
        (setf (gir-window new-lisp-window) new-gtk4-window
              (gethash (window-hkey new-gtk4-window) (windows lisp-app)) new-lisp-window))
      ;; we still need better way of creating simulated windows
      (let* ((new-lisp-window (make-instance window-class))
             (new-sim-window window-title))
        (setf (gir-window new-lisp-window) window-title
              (gethash (window-hkey new-sim-window) (windows lisp-app)) new-lisp-window))))

(defun window-creation-from-simulation (window-title window-class)
  (assert (symbolp window-title))
  (let ((new-window (window-activation-and-connection
                     *lisp-app*
                     nil
                     window-title
                     nil
                     window-class)))
    (setf (current-focus *lisp-app*) (gir-window new-window))
    new-window))

(defun window-creation-from-menu (window-title &optional window-menu-fn window-class)
  (let ((new-window (window-activation-and-connection
                     *lisp-app*
                     (gtk4-app *lisp-app*)
                     window-title
                     window-menu-fn
                     window-class)))
    (setf (current-focus *lisp-app*) (gir-window new-window))
    new-window))

(defun window-creation-from-main (app window-title &optional window-menu-fn window-class)
  (gtk4:connect app "activate"
                (lambda (app)
                  (let ((new-window (window-activation-and-connection
                                     *lisp-app*
                                     app
                                     window-title
                                     window-menu-fn
                                     window-class)))
                    (setf (current-focus *lisp-app*) (gir-window new-window))
                    new-window))))

(defun window ()
      (let ((app (gtk:make-application :application-id "org.bigos.gtk4-example.better-menu"
                                       :flags gio:+application-flags-flags-none+)))
        (setf *lisp-app* (make-instance 'lisp-app :gtk4-app app))
        (window-creation-from-main app *initial-title* *client-fn-menu-bar* *window-class*)

        (let ((status (gtk:application-run app nil)))
          (gobj:object-unref app)
          (setf *lisp-app*  nil)
          status)))
