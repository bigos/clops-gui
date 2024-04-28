;;; ============================ package gui-window ============================
(in-package #:gui-window)

(defparameter *menu-bar-menu-fn* nil)
(defparameter *draw-objects-fn* nil)
(defparameter *initial-window-width* 400)
(defparameter *initial-window-height* 200)
(defparameter *lisp-app* nil)

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
  (funcall *draw-objects-fn* (window-get *lisp-app* (gtk4:widget-parent
                                                     (gtk4:widget-parent
                                                      (gir:build-object-ptr (gir:nget gtk4:*ns* "DrawingArea") area)))))
  ;; ############################################################################

  ;; gtk will put the drawn surface on canvas
  )

(defun simulate-draw-func (window)
  (let* ((surface (cairo:create-image-surface :argb32
                                              (car (dimensions window))
                                              (cdr (dimensions window)))))

    ;; #########################################################################
    (setf  cairo:*context* (cairo:create-context surface))
    ;; call actual drawing
    (funcall *draw-objects-fn* window)
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
   (windows :std (make-hash-table)) ; see hasher, the hash keys is a symbol or integer
   (current-window :std nil  :type (or null cffi:foreign-pointer keyword))))

(defclass/std lisp-window ()
  ((gir-window  :type (or gir::object-instance keyword)
                :documentation "Either gir window or symbol used in test drawing")
   (children :documentation "Window can be divided into multiple sections with different content")
   (current-child :type (or null box))
   (hovered)
   (mouse-coordinates)
   (dimensions)))

;;; ========================== window manipulation =============================
;;; either the integer or testing keyword being the key of (gethash window (windows lisp-app))
(defmethod hasher ((window sb-sys:system-area-pointer))
  (cffi:pointer-address window))
(defmethod hasher ((window gir::object-instance))
  (cffi:pointer-address (gir::this-of window)))
(defmethod hasher ((window symbol))
  window)

(defun window-assert (window)
  (assert (or (typep window 'gir::object-instance)
              (typep window 'sb-sys:system-area-pointer)
              (typep window 'symbol))))

(defmethod window-add :before ((app lisp-app) (window gir::object-instance))
  (assert (equal "ApplicationWindow"
                 (gir:info-get-name (gir::info-of (gir:gir-class-of window))))))

(defmethod window-add ((app lisp-app) (window T))
  (window-assert window)
  (setf (gethash (hasher window)
                 (windows app))
        (make-instance 'lisp-window
                       :gir-window window)))

(defmethod window-get ((app lisp-app) (window T))
  (window-assert window)
  (gethash (hasher window)
           (windows app)))

(defmethod window-remove ((app lisp-app) (window T))
  (window-assert window)
  (if (remhash (hasher window) (windows app))
      (warn "success, window removed ~S" window)
      (warn "strange, window not removed ~S" window)))

;; =========================== dialogs =========================================
(defun present-about-dialog ()
  (let ((dialog (about-dialog)))
    (setf (gtk4:window-modal-p dialog) t
          (gtk4:window-transient-for dialog) (gtk4:application-active-window (gtk4-app *lisp-app*)))
    (gtk4:window-present dialog)))

(defun about-dialog ()
  (let ((dialog (gtk4:make-about-dialog)))
    (setf (gtk4:about-dialog-authors      dialog)   (list "Jacek Podkanski")
          (gtk4:about-dialog-website      dialog)   "https://github.com/bigos"
          (gtk4:about-dialog-program-name dialog)   "CLops"
          (gtk4:about-dialog-comments     dialog)   "Common Lisp Overly Premature System"
          (gtk4:about-dialog-license      dialog)   "GPL-3.0-or-later"
          (gtk4:about-dialog-system-information dialog) (format nil "~A" (uiop/os:implementation-identifier))
          (gtk4:about-dialog-logo-icon-name dialog) "application-x-addon")
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
(defun window-events (window)
  (let ((key-controller   (gtk4:make-event-controller-key))
        (focus-controller (gtk4:make-event-controller-focus)))

    (gtk4:widget-add-controller window key-controller)
    (gtk4:widget-add-controller window focus-controller)

    (gtk4:connect key-controller "key-pressed"
                  (lambda (e &rest args)
                    (declare (ignore e))
                    (apply #'gui-events:de-key-pressed (funcall #'translate-key-args args))))

    (gtk4:connect key-controller "key-released"
                  (lambda (e &rest args)
                    (declare (ignore e))
                    (apply #'gui-events:de-key-released (funcall #'translate-key-args args))))

    (gtk4:connect focus-controller "enter"
                  (lambda (e &rest args)
                    (declare (ignore e args))
                    (apply #'gui-events:de-focus-enter (list window))))

    (gtk4:connect focus-controller "leave"
                  (lambda (e &rest args)
                    (declare (ignore e args))
                    (apply #'gui-events:de-focus-leave (list window)))))

  (glib:timeout-add 1000
                    (lambda (&rest args)
                      (declare (ignore args))
                      (funcall #'gui-events:de-timeout)
                      glib:+source-continue+))

  (gtk4:connect window "close-request" (lambda (widget &rest args)
                                         (declare (ignore widget args))
                                         (window-remove *lisp-app* window)
                                         (gtk4:window-close window))))

(defun canvas-events (canvas)
  (labels
      ((canvas-window (c)
         (gtk4:widget-parent (gtk4:widget-parent c))))
    (let ((motion-controller (gtk4:make-event-controller-motion)))
      (gtk4:widget-add-controller canvas motion-controller)

      (gtk4:connect motion-controller "motion"
                    (lambda (e &rest args) (declare (ignore e)) (apply #'gui-events:de-motion
                                                                       (append args
                                                                               (list
                                                                               (canvas-window
                                                                                canvas))))))
      (gtk4:connect motion-controller "enter"
                    (lambda (e &rest args) (declare (ignore e)) (apply #'gui-events:de-motion-enter
                                                                       (append args
                                                                               (list
                                                                                (canvas-window
                                                                                 canvas))))))
      (gtk4:connect motion-controller "leave"
                    (lambda (e &rest args) (declare (ignore e)) (apply #'gui-events:de-motion-leave
                                                                       (append args
                                                                               (list
                                                                                (canvas-window
                                                                                 canvas)))))))

    (let ((scroll-controller (gtk4:make-event-controller-scroll :flags gtk4:+event-controller-scroll-flags-vertical+)))
      (gtk4:widget-add-controller canvas scroll-controller)
      (gtk4:connect scroll-controller "scroll"
                    (lambda (e &rest args) (declare (ignore e)) (apply #'gui-events:de-scroll
                                                                       args))))

    (let ((gesture-click-controller (gtk4:make-gesture-click))
          (click-fn (lambda (event args click-de-fn)
                      (let ((current-button (gtk4:gesture-single-current-button event)))
                        (apply click-de-fn (list current-button
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
                                    (gui-events:de-resize (first args) (second args) (canvas-window
                                                                           canvas))))))

;; =============================================================================

(defun new-window-for-app (app)
    (let ((window (gtk4:make-application-window :application app)))
      (gtk4:application-add-window app window)

      (setf
       (gtk4:application-menubar app) (funcall *menu-bar-menu-fn* app)
       (gtk4:application-window-show-menubar-p window) T)

      (setf
       (gtk4:window-title window) "CLOS Toy"
       (gtk4:window-default-size window) (list *initial-window-width*
                                               *initial-window-height*))

      (let ((box (gtk4:make-box :orientation gtk4:+orientation-vertical+
                                :spacing 0)))
        (let ((canvas (gtk4:make-drawing-area)))

          (setf (gtk4:widget-vexpand-p canvas) T
                (gtk4:drawing-area-draw-func canvas) (list (cffi:callback %draw-func)
                                                           (cffi:null-pointer)
                                                           (cffi:null-pointer)))
          (canvas-events canvas)
          (gtk4:box-append box canvas))
        (setf (gtk4:window-child window) box))

      (window-events window)

      (format t "actions defined for app ~A~%"  (gio:action-group-list-actions app))

      (gtk:window-present window)

      window))

(defun window-activation (app)
  (gtk4:connect app "activate"
                (lambda (app)
                  (window-add *lisp-app* (new-window-for-app app)))))

(defun window ()
  (let ((app (gtk:make-application :application-id "org.bigos.gtk4-example.better-menu"
                                   :flags gio:+application-flags-flags-none+)))
    (setf *lisp-app* (make-instance 'lisp-app :gtk4-app app))
    (window-activation app)

    (let ((status (gtk:application-run app nil)))
      (gobj:object-unref app)
      (setf *lisp-app*  nil)
      status)))
