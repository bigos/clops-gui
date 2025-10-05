(declaim (optimize (speed 0) (safety 3) (debug 3)))

;; (ql:quickload :clops-gui)

(in-package #:gui-window-gtk)


(defparameter *timeout-period* 1000)
(defparameter *initial-window-width* 400)
(defparameter *initial-window-height* 200)
(defparameter *client-fn-menu-bar* nil)
(defparameter *initial-title* "change me")

;; =========================== dialogs =========================================
(cffi:defcallback %save-func :void ((source-object :pointer)
                                    (res :pointer)
                                    (data :pointer))
  (declare (ignore data))
  (warn "running file dialog save callback")

  (funcall
   (lambda ()
     (let* ((dialog (gobj:pointer-object source-object 'gtk4:file-dialog))
            (result (gobj:pointer-object res 'gio:async-result))
            (file (gtk4:file-dialog-save-finish dialog result)))
       (print (gio:file-uri file))))))

(defun present-file-save-dialog ()
  (let ((file-dialog (gtk4:make-file-dialog)))
    (warn "running file dialog save")
    (gtk4:file-dialog-save file-dialog
                           (cffi:null-pointer)
                           (cffi:null-pointer)
                           (cffi:callback %save-func)
                           (cffi:null-pointer))))

(cffi:defcallback %open-func :void ((source-object :pointer)
                                    (res :pointer)
                                    (data :pointer))
  (declare (ignore data))
  (warn "running file dialog open callback")

  (funcall
   (lambda ()
     (let* ((dialog (gobj:pointer-object source-object 'gtk4:file-dialog))
            (result (gobj:pointer-object res 'gio:async-result))
            (file (gtk4:file-dialog-open-finish dialog result)))
       (print (gio:file-uri file))))))

(defun present-file-open-dialog ()
  (let ((file-dialog (gtk4:make-file-dialog)))
    (warn "running file dialog open")
    (gtk4:file-dialog-open file-dialog
                           (cffi:null-pointer)
                           (cffi:null-pointer)
                           (cffi:callback %open-func)
                           (cffi:null-pointer))))


(defun present-about-dialog (data)
  (let ((dialog (about-dialog data)))
    (setf (gtk4:window-modal-p dialog) t)
    (setf (gtk4:window-transient-for dialog) (gtk4:application-active-window (gui-app:gtk4-app gui-app:*lisp-app*)))
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
  (loop for aw = (gtk4:application-active-window (gui-app:gtk4-app gui-app:*lisp-app*))
        until (null aw)
        do (gtk4:window-close aw)))

;; ============================== app windows ==================================

(defun app-windows ()
  (when (gui-app:gtk4-app gui-app:*lisp-app*)
    (let ((app-windows (gtk4:application-windows (gui-app:gtk4-app gui-app:*lisp-app*))))
      (loop for pos from 0 below (glib:glist-length app-windows)
            collect (glib:glist-nth app-windows pos)))))

;; =#.============================ key event translation =========================
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
                    (setf (gui-app:current-focus gui-app:*lisp-app*) window)
                    (apply #'gui-events:de-key-pressed
                           (append (list lisp-window)
                                   (funcall #'translate-key-args args)))))

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
                    (setf (gui-app:current-focus gui-app:*lisp-app*) window)
                    (apply #'gui-events:de-focus-enter (list lisp-window))))

    (gtk4:connect focus-controller "leave"
                  (lambda (e &rest args)
                    (declare (ignore e args))
                    (setf (gui-app:current-focus gui-app:*lisp-app*) nil)
                    (apply #'gui-events:de-focus-leave (list lisp-window)))))

  (glib:timeout-add *timeout-period*
                    (lambda (&rest args)
                      (declare (ignore args))
                      (funcall #'gui-events:de-timeout lisp-window)
                      glib:+source-continue+))

  (gtk4:connect window "close-request" (lambda (widget &rest args)
                                         (declare (ignore widget args))
                                         (gui-app:window-remove gui-app:*lisp-app* window)
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
                                                                               args))))
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
      (setf (gtk4:application-menubar app) (funcall window-menu-fn app lisp-window))
      (setf (gtk4:application-window-show-menubar-p window) T))

    (setf (gtk4:window-title window) window-title)
    (setf (gtk4:window-default-size window) (list *initial-window-width*
                                                  *initial-window-height*))

    (let ((box (gtk4:make-box :orientation gtk4:+orientation-vertical+
                              :spacing 0)))
      (let ((canvas (gtk4:make-drawing-area)))

        (setf (gtk4:widget-vexpand-p canvas) T
              (gtk4:drawing-area-draw-func canvas) (list (cffi:callback gui-drawing:%draw-func)
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
  (if gtk4-app
      (let ((new-gtk4-window (new-window-for-app gtk4-app window-title window-menu-fn lisp-window)))
        (setf (gui-window:gir-window lisp-window) new-gtk4-window)
        (setf (gethash (gui-window:window-hkey new-gtk4-window) (gui-app:windows lisp-app)) lisp-window))
      ;; we still need better way of creating simulated windows
      (let ((new-sim-window window-title))
        (setf (gui-window:gir-window lisp-window) new-sim-window)
        (setf (gethash (gui-window:window-hkey new-sim-window) (gui-app:windows lisp-app)) lisp-window))))

(defun window-creation-from-simulation (window-title lisp-window)
  (assert (symbolp window-title))
  (let ((new-window (window-activation-and-connection
                     gui-app:*lisp-app*
                     nil
                     window-title
                     nil
                     lisp-window)))
    (setf (gui-app:current-focus  gui-app:*lisp-app*) (gui-window:gir-window new-window))
    new-window))

(defun window-creation-from-menu (window-title &optional window-menu-fn lisp-window)
  (let ((new-window (window-activation-and-connection
                     gui-app:*lisp-app*
                     (gui-app:gtk4-app gui-app:*lisp-app*)
                     window-title
                     window-menu-fn
                     lisp-window)))
    (setf (gui-app:current-focus gui-app:*lisp-app*) (gui-window:gir-window new-window))
    new-window))

(defun window-creation-from-main (app window-title &optional window-menu-fn lisp-window)
  (gtk4:connect app "activate"
                (lambda (app)
                  (let ((new-window (window-activation-and-connection
                                     gui-app:*lisp-app*
                                     app
                                     window-title
                                     window-menu-fn
                                     lisp-window)))
                    (setf (gui-app:current-focus gui-app:*lisp-app*) (gui-window:gir-window new-window))
                    new-window))))

(defun window-main (lisp-window)
      (let ((app (gtk:make-application :application-id "org.bigos.gtk4-example.better-menu"
                                       :flags gio:+application-flags-flags-none+)))
        (setf gui-app:*lisp-app* (gui-app:make-lisp-app app))
        (window-creation-from-main app *initial-title* *client-fn-menu-bar* lisp-window)

        (let ((status (gtk:application-run app nil)))
          (gobj:object-unref app)
          (setf gui-app:*lisp-app*  nil)
          status)))
