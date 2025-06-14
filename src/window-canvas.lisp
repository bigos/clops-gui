(declaim (optimize (speed 0) (safety 3) (debug 3)))

(in-package #:window-canvas)

;;; ================ common module =====================================================
;;; @@@
(defparameter *gtk-client-fn-draw-objects* nil)
(defparameter *gtk-app* nil)
(defparameter *gtk-timeout-period* 1000)
;;; @@@
(defparameter *gtk-initial-window-width* 400)
;;; @@@
(defparameter *gtk-initial-window-height* 200)
;;; @@@
(defparameter *gtk-client-fn-menu-bar* nil)
;;; @@@
(defparameter *gtk-client-fn-process-event* nil)
;;; @@@
(defparameter *window-size-cons-code* nil)
;;; @@@
(defparameter *gtk-initial-title* "change me")
(defparameter *all-windows* (make-hash-table))

;;; ============================ colours ===============================================
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

;;; @@@ file:~/Programming/Lisp/clops-gui/src/gui-window.lisp::5
(defun set-rgba (color)
  (let ((parsed-color (color-to-rgba color)))
    (if (first parsed-color)
        (apply 'cairo:set-source-rgba (rest parsed-color))
        (error "~S is not a valid color" color))))

;;; ================= classes ==================================================
;;; @@@ file:~/Programming/Lisp/clops-gui/src/gui-window.lisp::8
(defclass/std lisp-window ()
  ((gir-window  :type (or gir::object-instance keyword)
                :documentation "Either gir window object or keyword used in test drawing")))

;;; ================================= All defined GUI Events ===========================

(defun de-menu-simple  (lisp-window action-name)           (process-event lisp-window :menu-simple action-name))
(defun de-menu-bool    (lisp-window action-name bool01)    (process-event lisp-window :menu-bool   action-name bool01))
(defun de-menu-radio   (lisp-window action-name radiostr)  (process-event lisp-window :menu-radio  action-name radiostr))
(defun de-motion       (lisp-window x y)                   (process-event lisp-window :motion       x y))
(defun de-motion-enter (lisp-window x y)                   (process-event lisp-window :motion-enter x y))
(defun de-focus-enter  (lisp-window)                       (process-event lisp-window :focus-enter  ))
(defun de-motion-leave (lisp-window)                       (process-event lisp-window :motion-leave))
(defun de-focus-leave  (lisp-window)                       (process-event lisp-window :focus-leave))
(defun de-pressed      (lisp-window button x y)            (process-event lisp-window :pressed   button x y))
(defun de-released     (lisp-window button x y)            (process-event lisp-window :released  button x y))
(defun de-scroll       (lisp-window dx dy)                 (process-event lisp-window :scroll    dx dy))
(defun de-key-pressed  (lisp-window letter name code mods) (process-event lisp-window :key-pressed   letter name code mods))
(defun de-key-released (lisp-window letter name code mods) (process-event lisp-window :key-released  letter name code mods))
(defun de-resize       (lisp-window width height)          (process-event lisp-window :resize width height))
(defun de-timeout      (lisp-window )                      (process-event lisp-window :timeout))

(defun process-event (lisp-window event &rest args)
  (if (null *gtk-client-fn-process-event*)
      (error "implement process-event")
      (funcall *gtk-client-fn-process-event* lisp-window event args)))

;;; both %draw-func and simulate-draw-func create context on each call
;;; we need to investigate whether it is necessary only on
;;; the dimensions change or moving the window

;;; drawing callback ===========================================================
;; file:~/Programming/Lisp/clops-gui/src/gui-drawing.lisp::12
(cffi:defcallback %draw-func :void ((area :pointer)
                                    (cr :pointer)
                                    (width :int)
                                    (height :int)
                                    (data :pointer))
  (declare (ignore area data))

  ;; create context
  (setf cairo:*context* (make-instance 'cairo:context
                                       :pointer cr
                                       :width width
                                       :height height
                                       :pixel-based-p nil))

  ;; version with gir object of the window,for window identification and attributes

  ;; TODO just figure out how to pass dimentions and some kind of id
  (funcall *gtk-client-fn-draw-objects*
           (cffi:pointer-address
            (gir::this-of
             (gtk4:widget-parent
              (gtk4:widget-parent
               (gir:build-object-ptr (gir:nget gtk4:*ns* "DrawingArea") area))))))

  ;; call actual drawing - on the context's gtk4 canvas surface
  ;; (funcall *gtk-client-fn-draw-objects*)
  )

;;; @@@ file:~/Programming/Lisp/clops-gui/src/gui-drawing.lisp::31
(defun simulate-draw-func (window &optional log)
  (assert (typep window 'lisp-window))
  (assert (typep (gir-window window) 'keyword))

  (let ((window-size-cons
          (if *window-size-cons-code*
              (funcall *window-size-cons-code*)
              (progn
                (warn "Using default dimensions, provide a function that returns (cons w h)")
                (cons 150 150)))))

    (unless (and (consp window-size-cons)
                 (integerp (car window-size-cons))
                 (integerp (cdr window-size-cons)))
      (error "*window-size-cons-code* should return a pair of integers but got ~S" window-size-cons))

    (let ((surface (cairo:create-image-surface :argb32
                                               (car window-size-cons)
                                               (cdr window-size-cons))))

      ;; create context
      (setf  cairo:*context* (cairo:create-context surface))

      ;; call actual drawing - on the surface
      (if *gtk-client-fn-draw-objects*
          (funcall *gtk-client-fn-draw-objects* (gir-window window))
          (error "You forgot to declare drawing function in *client-fn-draw-objects* "))

      ;; put drawn surface to a file
      (cairo:surface-write-to-png surface
                                  (format nil "~Acairo-simulate-drawing~A-~A-~A.png"
                                          (uiop:temporary-directory)
                                          (get-internal-run-time)
                                          :simulated-window
                                          log)))))

;;; @@@ file:~/Programming/Lisp/clops-gui/src/gui-window.lisp::17
(defmethod redraw-canvas ((window lisp-window) &optional log)
  (etypecase (gir-window window)
    (keyword
     (simulate-draw-func window log))
    (gir::object-instance
     (gtk4:widget-queue-draw
      (serapeum:~> window gir-window gtk4:widget-first-child gtk4:widget-first-child)))))

;; =========================== dialogs =========================================
;; file:~/Programming/Lisp/clops-gui/src/gui-window-gtk.lisp::14
(defun present-about-dialog (data)
  (let ((dialog (about-dialog data)))
    (setf (gtk4:window-modal-p dialog) t)
    (setf (gtk4:window-transient-for dialog)
          (gtk4:application-active-window *gtk-app*)
          ;; (error "implement finding active window")
          )
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

;; ============================== app windows ==================================
;; file:~/Programming/Lisp/clops-gui/src/gui-window-gtk.lisp::40
(defun app-windows ()
  (when *gtk-app*
    (let ((app-windows (gtk4:application-windows *gtk-app*)))
      (loop for pos from 0 below (glib:glist-length app-windows)
            collect (glib:glist-nth app-windows pos)))))

;; file:~/Programming/Lisp/clops-gui/src/gui-window-gtk.lisp::33
(defun close-all-windows-and-quit ()
  (loop for aw = (gtk4:application-active-window  *gtk-app*)
        until (null aw)
        do (gtk4:window-close aw)))

;;; @@@
(defun get-window    (window-id)
  (gethash window-id *all-windows*))

(defun add-window    (window-id lisp-window)
  (setf (gethash window-id *all-windows*) lisp-window))

(defun remove-window (window-id)
  (if (remhash *all-windows* window-id)
      (warn "window removed ~S" window-id)
      (warn "strange, could not remove window ~S" window-id)))

;;; @@@
(defun remove-all-windows ()
  (clrhash *all-windows*))

;; ============================= key event translation =========================
;; file:~/Programming/Lisp/clops-gui/src/gui-window-gtk.lisp::47
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
;;; file:~/Programming/Lisp/clops-gui/src/gui-window-gtk.lisp::71
(defun window-events (window lisp-window)
  (let ((key-controller   (gtk4:make-event-controller-key))
        (focus-controller (gtk4:make-event-controller-focus)))

    (gtk4:widget-add-controller window key-controller)
    (gtk4:widget-add-controller window focus-controller)

    (gtk4:connect key-controller "key-pressed"
                  (lambda (e &rest args)
                    (declare (ignore e))
                    ;; (setf (gui-app:current-focus *gtk-app*) window)
                    (apply #'de-key-pressed
                           (append (list lisp-window)
                                   (funcall #'translate-key-args args)))))

    (gtk4:connect key-controller "key-released"
                  (lambda (e &rest args)
                    (declare (ignore e))
                    (apply #'de-key-released
                           (append (list lisp-window)
                                   (funcall #'translate-key-args args)))))
    ;; for some reason enter and leave are not reliable and I need to add window as in key-pressed to some events
    (gtk4:connect focus-controller "enter"
                  (lambda (e &rest args)
                    (declare (ignore e args))
                    ;; (setf (gui-app:current-focus *gtk-app*) window)
                    (apply #'de-focus-enter (list lisp-window))))

    (gtk4:connect focus-controller "leave"
                  (lambda (e &rest args)
                    (declare (ignore e args))
                    ;; (setf (gui-app:current-focus *gtk-app*) nil)
                    (apply #'de-focus-leave (list lisp-window)))))

  (glib:timeout-add *gtk-timeout-period*
                    (lambda (&rest args)
                      (declare (ignore args))
                      (funcall #'de-timeout lisp-window)
                      glib:+source-continue+))

  (gtk4:connect window "close-request" (lambda (widget &rest args)
                                         (declare (ignore widget args))
                                         ;; (gui-app:window-remove *gtk-app* window)
                                         (gtk4:window-close window))))
;; file:~/Programming/Lisp/clops-gui/src/gui-window-gtk.lisp::116
(defun canvas-events (canvas lisp-window)
  (let ((motion-controller (gtk4:make-event-controller-motion)))
    (gtk4:widget-add-controller canvas motion-controller)

    (gtk4:connect motion-controller "motion"
                  (lambda (e &rest args) (declare (ignore e)) (apply #'de-motion
                                                                     (append (list lisp-window)
                                                                             args))))
    (gtk4:connect motion-controller "enter"
                  (lambda (e &rest args) (declare (ignore e)) (apply #'de-motion-enter
                                                                     (append (list lisp-window)
                                                                             args))))
    (gtk4:connect motion-controller "leave"
                  (lambda (e &rest args) (declare (ignore e)) (apply #'de-motion-leave
                                                                     (append (list lisp-window)
                                                                             args)))))

  (let ((scroll-controller (gtk4:make-event-controller-scroll :flags gtk4:+event-controller-scroll-flags-vertical+)))
    (gtk4:widget-add-controller canvas scroll-controller)
    (gtk4:connect scroll-controller "scroll"
                  (lambda (e &rest args) (declare (ignore e)) (apply #'de-scroll
                                                                     lisp-window
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
                  (lambda (event &rest args) (apply click-fn (list event args #'de-pressed))))
    (gtk4:connect gesture-click-controller "released"
                  (lambda (event &rest args) (apply click-fn (list event args #'de-released)))))


    ;; for some reason resize signal does not work without  notify

  (gtk4:connect canvas "notify" (lambda (widget &rest args)
                                  (format t "~&>>>>>>>>>>>>>>>>>>>>>>>> notifying ~S ~S~%" widget args)))

  (gtk4:connect canvas "resize" (lambda (widget &rest args)
                                  (declare (ignore widget))
                                  (de-resize lisp-window (first args) (second args)))))

;; =============================================================================
;; file:~/Programming/Lisp/clops-gui/src/gui-window-gtk.lisp::170
(defun new-window-for-app (app window-title window-menu-fn lisp-window)
  (let ((window (gtk4:make-application-window :application app)))
    (gtk4:application-add-window app window)

    (when window-menu-fn
      (setf (gtk4:application-menubar app) (funcall window-menu-fn app lisp-window))
      (setf (gtk4:application-window-show-menubar-p window) T))

    (setf (gtk4:window-title window) window-title)
    (setf (gtk4:window-default-size window) (list *gtk-initial-window-width*
                                                  *gtk-initial-window-height*))

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

(defun window-activation-and-connection (lisp-app gtk4-app window-title window-menu-fn lisp-window)
  (declare (ignore lisp-app))
  (if *gtk-app*
      (progn
        (let ((new-gtk4-window (new-window-for-app gtk4-app window-title window-menu-fn lisp-window)))
          ;; lisp window needs to know of its gtk4 window
          (setf (gir-window lisp-window) new-gtk4-window)

          (add-window (cffi:pointer-address (gir::this-of new-gtk4-window))
                      lisp-window)))

      ;; we still need better way of creating simulated windows
      (progn
        (setf (gir-window lisp-window) window-title)

        (add-window window-title
                    lisp-window))))

;;; @@@ file:~/Programming/Lisp/clops-gui/src/gui-window-gtk.lisp::212
(defun window-creation-from-simulation (window-title lisp-window)
  (assert (symbolp window-title))
  (let ((new-window (window-activation-and-connection
                     *gtk-app*
                     nil
                     window-title
                     nil
                     lisp-window)))
    new-window))

(defun window-creation-from-menu (window-title &optional window-menu-fn lisp-window)
  (let ((new-window (window-activation-and-connection
                     *gtk-app*
                     *gtk-app*
                     window-title
                     window-menu-fn
                     lisp-window)))
    new-window))

(defun window-creation-from-main (app window-title &optional window-menu-fn lisp-window)
  (let ((new-window (window-activation-and-connection
                     *gtk-app*
                     app
                     window-title
                     window-menu-fn
                     lisp-window)))
    new-window))

(defun window (lisp-window)
  (let ((app (gtk:make-application :application-id "org.bigos.gtk4-example.better-menu"
                                   :flags gio:+application-flags-flags-none+)))
    (setf *gtk-app* app)

    (gtk4:connect app
                  "activate"
                  (lambda (app)
                    (window-creation-from-main app *gtk-initial-title* *gtk-client-fn-menu-bar* lisp-window)))

    (let ((status (gtk:application-run app nil)))
      (gobj:object-unref app)
      (setf *gtk-app*  nil)
      status)))

;;; @@@
(defun window-main (lisp-window)
  (window lisp-window))
