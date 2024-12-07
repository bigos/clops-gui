(declaim (optimize (speed 0) (safety 3) (debug 3)))

#| loading |#

;; (load "~/Programming/Lisp/clops-gui/examples/counter-third.lisp")
(push #p "~/Programming/Lisp/clops-gui/" ql:*local-project-directories*)
(ql:quickload '(:clops-gui :access :serapeum :defclass-std) :silent nil)

#| planning

  ** planning

  *** event is processed, either Gtk4 or simulated event
  that is decided on the type of the window attribute used in the event
  file:~/Programming/Lisp/clops-gui/examples/counter-second.lisp::157

  **** application state is updated

  **** relevant widgets are updated

  **** Layout is updated

  *** redraw-canvas is called
  file:~/Programming/Lisp/clops-gui/src/gui-window.lisp::17
  depending if real gtk4 or simulated event based on window properties
  we either
  **** enqueue gtk4 drawing which calls back to draw-func
  file:~/Programming/Lisp/clops-gui/src/gui-drawing.lisp::12

  **** or simulate drawing
  file:~/Programming/Lisp/clops-gui/src/gui-drawing.lisp::34

  *** funcall draw-window
  file:~/Programming/Lisp/clops-gui/examples/counter-second.lisp::114

  *** wait for the next event

  |#

;;; --------------------- package ----------------------------------------------
(defpackage #:counter-third
  (:use #:cl)
  (:import-from :serapeum
   :~>)
  (:import-from :defclass-std
   :defclass/std))

(in-package #:counter-third)

;;; -------------------------------- code --------------------------------------
(defclass/std counter-third-window (gui-window:lisp-window)
  (()))

(defclass/std individual ()
  ((id :r :allocation :instance)
   (id-count :r :allocation :class :std 0)
   (ids :allocation :class :std (make-hash-table))))

(defclass/std node (individual)
  ((parent-id)
   (children-ids)))

(defmethod initialize-instance :after ((individual individual) &key)
  "After initialising increase class allocated ID."
  (setf (slot-value individual 'id-count) (1+ (id-count individual)))
  (setf (slot-value individual 'id)       (1+ (id-count individual)))
  (setf (gethash (id individual) (ids individual)) individual)

  ;; that does not work
  (let ((hid (slot-value individual 'id)))
    (sb-ext:finalize individual (lambda ()
                                  (warn "removing ~S" individual)
                           (remhash hid (slot-value individual 'ids))))))


;; (defparameter zzz  (loop for x from 1 to 5 collect  (make-instance 'node)))

#|

removing ids from individual

http://www.sbcl.org/manual/index.html#Finalization

file:///home/jacek/Documents/Manuals/Lisp/HyperSpec-7-0/HyperSpec/Body/m_defi_3.htm#define-setf-expander
file:///home/jacek/Documents/Manuals/Lisp/HyperSpec-7-0/HyperSpec/Body/m_defset.htm#defsetf

|#

;;; ============================================================================
;;; ---------------------------------- draw window -----------------------------
(defmethod draw-window ((window counter-third-window))
  ;; paint background
  (let ((cv 0.95)) (cairo:set-source-rgb  cv cv cv))
  (cairo:paint)

  (cairo:select-font-face "Ubuntu Mono" :italic :bold)
  (cairo:set-font-size 20)
  (cairo:move-to 20 30)
  (gui-window:set-rgba "black")
  (cairo:show-text (format nil "try to code something")))

(defmethod draw-window :after ((window counter-third-window))
  ;; pink square follows the mouse
  (let ((app gui-app:*lisp-app*))
    (when (and (eq (gui-app:current-motion app)
                   window)
               (gui-app:mouse-coordinates app))
      (gui-window:set-rgba "pink")
      (cairo:rectangle
       (car (gui-app:mouse-coordinates app))
       (cdr (gui-app:mouse-coordinates app))
       25
       25)
      (cairo:fill-path))))

;;; -------------------------------- process event -----------------------------
(defmethod process-event ((lisp-window counter-third-window) event &rest args)
  (unless (member event '(:timeout :motion))
    (warn "event ~S ~S" event args))

  (case event
    (:timeout
     ;; do nothing yet
     )
    ((:motion :motion-enter)
     ;; we use simple case with one window so we ignore the window argument
     (destructuring-bind ((x y)) args
       ;; (update-mouse-location *model* x y)
       (gui-app:mouse-motion-enter lisp-window x y)))
    (:motion-leave
     (gui-app:mouse-motion-leave))
    (:focus-enter)
    (:focus-leave)
    (:pressed
     (destructuring-bind ((button x y)) args
       (declare (ignore button x y))
       ;; (update-mouse-press *model* x y button)
       ))
    (:released
     ;; (update-mouse-release *model*)
     (gui-app:mouse-button-released))
    (:scroll)
    (:resize
     (destructuring-bind ((w h)) args
       (gui-window:window-resize w h lisp-window)))
    (:key-pressed
     (destructuring-bind ((entered key-name key-code mods)) args
       (format t "~&>>> key pressed ~S~%" (list entered key-name key-code mods))
       (warn "model ~S" *model*)
       ))
    (otherwise
     (warn "not handled event ~S ~S" event args)))

  ;; moving widgets -------------------------
  ;; (warn "may implement moving widgets in response to actions)

  ;; redrawing ------------------------------
  (gui-window:redraw-canvas lisp-window (format  nil "~A" event)))

;;; ============================================================================
(defun main ()

  (setf
   gui-drawing:*client-fn-draw-objects*  'counter-third::draw-window
   gui-window-gtk:*client-fn-menu-bar*      nil
   gui-events:*client-fn-process-event* 'counter-third::process-event
   gui-window-gtk:*initial-window-width*    600
   gui-window-gtk:*initial-window-height*   400
   gui-window-gtk:*initial-title*           "Counter Third")

  (gui-window-gtk:window (make-instance 'counter-third-window)))


#| mini spec
  **** hmm
  We need scaled counter the screen is divided into 2 sections, upper 2/3 is for
  counter and bottom 1/3 is for echo area.

  The counter sub section is divided horizontally into 3 sections, plus-button,
  count and minus-button.

  depending on the window size, the counter sub section elements will scale in a
  reactive way, hiding the buttons when the width goes below the small limit,
  limiting buttons to 10% at middle limit, and haveing 30% buttons at large limit.
 |#

;;; === test preparation =======================================================
(defun test-experiment-first-window ()
  (setf gui-drawing:*client-fn-draw-objects*  'counter-third::draw-window)

  (setf gui-app:*lisp-app* (gui-app:make-lisp-app))
  (assert (zerop (hash-table-count (gui-app:all-windows))))

  (let ((lisp-window (make-instance 'counter-third-window)))
    (gui-window-gtk:window-creation-from-simulation :testing lisp-window)
    (assert (eq 1 (hash-table-count (gui-app:all-windows))))
    lisp-window))

(defun test-experiment ()
  (warn "starting test-experiment")
  (let ((win (test-experiment-first-window)))
    (process-event win :RESIZE '(600 400))
    (process-event win :KEY-RELEASED '("" "Return" 36 NIL))
    (process-event win :TIMEOUT NIL))


  (warn "finished test-experiment"))

;; (test-experiment)
