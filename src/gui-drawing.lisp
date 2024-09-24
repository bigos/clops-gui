(in-package #:gui-drawing)

(defparameter *client-fn-draw-objects* nil)


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

(defun simulate-draw-func (window &optional log)
  (let ((surface (cairo:create-image-surface :argb32
                                              ;; use defaults if dimensions are nil
                                              (or (car (dimensions window)) 150)
                                              (or (cdr (dimensions window)) 100))))

    ;; #########################################################################
    (setf  cairo:*context* (cairo:create-context surface))

    (when (null *client-fn-draw-objects*)
      (error "You forgot to declare drawing function in *client-fn-draw-objects* "))

    ;; call actual drawing
    (funcall *client-fn-draw-objects* window)
    ;; #########################################################################

    ;; put drawn surface to a file
    (cairo:surface-write-to-png surface
                                (format nil "~Acairo-simulate-drawing~A-~A-~A.png"
                                        (uiop:temporary-directory)
                                        (get-internal-run-time)
                                        (gir-window window)
                                        log))))
