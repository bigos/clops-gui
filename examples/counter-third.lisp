(declaim (optimize (speed 0) (safety 3) (debug 3)))

#| loading |#

;; (load "~/Programming/Lisp/clops-gui/examples/counter-third.lisp")
(push #p "~/Programming/Lisp/clops-gui/" ql:*local-project-directories*)
(ql:quickload '(:clops-gui
                :serapeum
                :defclass-std
                :cl-containers)
              :silent nil)

#| planning
  libraries
  https://cl-containers.common-lisp.dev/documentation/index.html
  https://cl-containers.common-lisp.dev/documentation/metabang.cl-containers-package/index.html

  https://github.com/orthecreedence/cl-hash-util

  https://github.com/AccelerationNet/access/blob/a4f87fc1007f5c9a0a2abfddc1c23a77e87096f8/access.lisp#L458

  https://github.com/CodyReichert/awesome-cl?tab=readme-ov-file#data-structures

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
   :~> :@)
  (:import-from :defclass-std
   :defclass/std))

(in-package #:counter-third)

;;; ------------------------------- macros -------------------------------------
(defmacro assign (place value0)
  (let ((value (gensym "VALUE")))
    `(let ((,value ,value0))
         (progn
           (format t "assigning place of type ~S and value ~S with value ~S~%"
                   (type-of ,place) ,place ,value)
           (typecase ,place
             (null
              (progn
                (format t "ASSIGN initializing with value ~S~%" ,value)
                (setf ,place ,value)))
             (standard-object
              (progn
                (format t "ASSIGN updating ~S~%" (type-of ,place))
                (cond ((null ,value)
                       (progn
                         (format t "ASSIGN destroying object~%")
                         (destroy-object ,place)))
                      (T
                       (progn
                         (format t "ASSIGN warning assigning with another value~%")
                         (setf ,place ,value))))))
             (t (progn
                  (format t "ASSIGN doing any~%")
                  (if (null ,value)
                      (progn
                        (format t "ASSIGN assigning with null~%")
                        (setf,place ,value))
                      (setf ,place ,value)))))))))

(defmethod destroy-object ((node node))
  (remhash (id node) (ids node))
  (setf node nil))


;;; -------------------------------- code --------------------------------------
(defclass/std counter-third-window (gui-window:lisp-window)
  (()))

(defclass/std model ()
  ((mouse-location)
   (mouse-button)
   (current-widget)
   (counted)
   (button-plus)
   (text)
   (button-minus)))

(defclass/std individual ()
  ((id :r :allocation :instance)
   (id-count :r :allocation :class :std 0)
   (ids         :allocation :class :std (make-hash-table))))

(defclass/std node (individual)
  ((parent-id)
   (children-ids)))

(defclass/std box (node)
  ((top-left)
   (width)
   (height)
   (mouse-over)))

(defclass/std button (box)
  ((label)))

(defclass/std text (box)
  ((label)))

;;; ==================== methods and functions =================================
(defmethod initialize-instance :after ((node node) &key)
  (warn "initilize-instance")
  (setf (slot-value node 'id-count) (1+ (id-count node)))
  (setf (slot-value node 'id) (id-count node) )
  (setf (@ (ids node) (id node)) node))

(defmethod reset-everything ((node node))
  (loop for k being the hash-key of (ids node) do
    (remhash k (ids node)))
  (setf (slot-value node 'id-count) 0)
  (setf (slot-value node 'id) 0)
  (setf (slot-value node 'ids) (make-hash-table)))

(defmethod print-object ((obj node) stream)
  (print-unreadable-object (obj stream :type t :identity t)
    (format stream "~a"
            (loop for sl in (sb-mop:class-slots (class-of obj))
                  for slot-name = (sb-mop:slot-definition-name sl)
                  collect (cons slot-name
                                (if (slot-boundp obj slot-name)
                                    (format nil "~S" (slot-value obj slot-name))))))))

(defmethod mouse-overp (model (box box))
  (let ((mx (car (~> model mouse-location)))
        (my (cdr (~> model mouse-location))))
    (and (>= mx (~> box top-left car))
         (>= my (~> box top-left cdr))
         (<= mx (+
                 (~> box top-left car)
                 (~> box width)))
         (<= my (+
                 (~> box top-left cdr)
                 (~> box height))))))

(defun most-specific-widget (model x y)
  (declare (ignore x y))
  (let ((widgets (list (~> model button-plus)
                       (~> model button-minus))))
    (or
     (when (mouse-overp model (first widgets))  (first widgets ))
     (when (mouse-overp model (second widgets)) (second widgets)))))

;;; ----------------------- update ---------------------------------------------
(defmethod update-mouse-over ((widget box))
  (setf (~> widget mouse-over) :mouse-over))

(defmethod update-mouse-out ((widget box))
  (setf (~> widget mouse-over) nil))

(defun update-mouse-location (model x y)
  (setf (~> model mouse-location) (cons x y))
  ;; depend on location update mouse over or mouse out
  ;; (break "investigate update mouse location")
  (let ((current-widget (~> model current-widget))
        (most-specific-widget (most-specific-widget model x y)))

    (if current-widget
        (update-mouse-out  current-widget)
        (progn
          (update-mouse-out (~> model button-plus))
          (update-mouse-out (~> model button-minus))))

    (when most-specific-widget
      (update-mouse-over most-specific-widget))))

(defun update-mouse-press (model x y button)
  (update-mouse-location model x y)
  (setf (~> model mouse-button) t)

  (when (eq 1 button)
    (let ((msw (most-specific-widget model x y)))

      (when msw
        (setf (~> msw mouse-over) :mouse-active))

      (cond ((null msw) (warn "null msw"))
            ((equal "+" (~> msw label))
             (warn "plusing ~S" (~> model counted))
             (setf (~> model counted) (1+ (~> model counted))))
            ((equal "-" (~> msw label))
             (warn "minusing")
             (setf (~> model counted) (1- (~> model counted))))
            (t
             (progn (warn "NOT setting zzz")))))))

(defun update-mouse-release (model x y button)
  (declare (ignore button))
  (update-mouse-location model x y)
  (setf (~> model mouse-button) nil)

  (let ((msw (most-specific-widget model x y)))
    (when msw
      (setf (~> msw mouse-over) :mouse-over))))

;;; ============================================================================
(defun draw-widget (w)
  (gui-window:set-rgba (if (null (~> w mouse-over))
                           "yellow"
                           (when (keywordp (~> w mouse-over))
                             (ecase (~> w mouse-over)
                               (:mouse-over "orange")
                               (:mouse-active "lime")))))

  (cairo:rectangle (~> w top-left (car _))
                   (~> w top-left (cdr _))
                   (~> w width)
                   (~> w height))
  (cairo:fill-path)

  (cairo:set-font-size 20)
  (cairo:move-to (~> w top-left (car _))
                 (+ 10 (~> w top-left (cdr _))))
  (gui-window:set-rgba "black")
  (cairo:show-text (format nil "~a" (~> w label))))

(defun draw-widget-count (model w)
  (gui-window:set-rgba (cond ((null (~> w mouse-over)) "yellow")
                             ((~> w mouse-over) "orange")
                             (T "red")))
  (cairo:rectangle (~> w top-left (car _))
                   (~> w top-left (cdr _))
                   (~> w width)
                   (~> w height))
  (cairo:fill-path)

  (cairo:set-font-size 20)
  (cairo:move-to (~> w top-left (car _))
                 (+ 10 (~> w top-left (cdr _))))
  (gui-window:set-rgba "black")
  (cairo:show-text (format nil "~a" (~> model counted))))

;;; ---------------------------------- draw window -----------------------------
(defmethod draw-window ((window counter-third-window))
  ;; paint background
  (let ((cv 0.95)) (cairo:set-source-rgb  cv cv cv))
  (cairo:paint)

  (cairo:select-font-face "Ubuntu Mono" :italic :bold)
  (cairo:set-font-size 20)
  (cairo:move-to 20 30)
  (gui-window:set-rgba "black")
  (cairo:show-text (format nil "try to code something"))

  (let ((w (~> *model* button-plus)))
    (draw-widget w))

  (let ((w (~> *model* text)))
    (draw-widget-count *model* w))

  (let ((w (~> *model* button-minus)))
    (draw-widget w)))

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
(defmethod process-gtk-event ((lisp-window counter-third-window) event &rest args)
  (unless (member event '(:timeout :motion))
    (warn "event ~S ~S" event args))

  (case event
    (:timeout
     ;; do nothing yet
     )
    ((:motion :motion-enter)
     ;; we use simple case with one window so we ignore the window argument
     (destructuring-bind ((x y)) args
       (update-mouse-location *model* x y)
       (gui-app:mouse-motion-enter lisp-window x y)))
    (:motion-leave
     (gui-app:mouse-motion-leave))
    (:focus-enter)
    (:focus-leave)
    (:pressed
     (destructuring-bind ((button x y)) args

       (update-mouse-press *model* x y button)

       ))
    (:released
     (destructuring-bind ((button x y)) args
       (update-mouse-release *model* x y button)
       (gui-app:mouse-button-released)))
    (:scroll)
    (:resize
     (destructuring-bind ((w h)) args
       (gui-window:window-resize w h lisp-window)))
    (:key-pressed
     (destructuring-bind ((entered key-name key-code mods)) args
       (format t "~&>>> key pressed ~S~%" (list entered key-name key-code mods))
       ;; (warn "model ~S" *model*)
       ))
    (otherwise
     (warn "not handled event ~S ~S" event args)))

  ;; moving widgets -------------------------
  ;; (warn "may implement moving widgets in response to actions)

  ;; redrawing ------------------------------
  (gui-window:redraw-canvas lisp-window (format  nil "~A" event)))

;;; ============================================================================
(defparameter *model* nil)

(defun init-model ()
  (setf *model* (make-instance 'model
                               :mouse-location nil
                               :current-widget nil
                               :counted  0
                               :button-plus  (make-instance 'button
                                                            :label "+"
                                                            :top-left (cons 10 10)
                                                            :width 50
                                                            :height 50)
                               :text         (make-instance 'text
                                                            :label 0
                                                            :top-left (cons 110 10)
                                                            :width 50
                                                            :height 50)
                               :button-minus (make-instance 'button
                                                            :label "-"
                                                            :top-left (cons 210 10)
                                                            :width 50
                                                            :height 50))))

;;; ============================================================================
(defun main ()
  (init-model)
  (progn
    (assign gui-drawing:*client-fn-draw-objects*  'counter-third::draw-window)
    (assign gui-window-gtk:*client-fn-menu-bar*      nil)
    (assign gui-events:*client-fn-process-event* 'counter-third::process-gtk-event)
    (assign gui-window-gtk:*initial-window-width*    600)
    (assign gui-window-gtk:*initial-window-height*   400)
    (assign gui-window-gtk:*initial-title*           "Counter Third"))

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
  (assign gui-drawing:*client-fn-draw-objects*  'counter-third::draw-window)

  (assign gui-app:*lisp-app* (gui-app:make-lisp-app))
  (assert (zerop (hash-table-count (gui-app:all-windows))))

  (let ((lisp-window (make-instance 'counter-third-window)))
    (gui-window-gtk:window-creation-from-simulation :testing lisp-window)
    (assert (eq 1 (hash-table-count (gui-app:all-windows))))
    lisp-window))

(defun test-experiment ()
  (warn "starting test-experiment")
  (init-model)
  (let ((win (test-experiment-first-window)))
    (process-gtk-event win :RESIZE '(600 400))
    (process-gtk-event win :KEY-RELEASED '("" "Return" 36 NIL))
    (process-gtk-event win :TIMEOUT NIL)

    (process-gtk-event win :MOTION-ENTER '(1.0 1.0))

    (assert (null (~> *model* button-plus mouse-over)))
    (process-gtk-event win :MOTION '(20.0 20.0))
    (assert (eq :mouse-over (~> *model* button-plus mouse-over)))


    (process-gtk-event win :PRESSED '(1 20.0 20.0))
    (assert (eq :mouse-active (~> *model* button-plus mouse-over)))
    (assert (eq 1 (~> *model* counted)) nil "counted must be 1")
    (process-gtk-event win :RELEASED '(1 20.0 20.0))
    (assert (eq :mouse-over (~> *model* button-plus mouse-over)))

    (process-gtk-event win :MOTION-ENTER '(1.0 1.0))
    (assert (null (~> *model* button-plus mouse-over)))

    (process-gtk-event win :MOTION '(220.0 20.0))
    (assert (null (~> *model* button-plus mouse-over)))

    (process-gtk-event win :PRESSED '(1 220.0 20.0))
    (assert (eq 0 (~> *model* counted)) nil "counted must be 0")
    (process-gtk-event win :RELEASED '(1 220.0 20.0)))

  (warn "finished test-experiment"))

;;; my assign macro is better than all those defsetfs
(defun test-node ()
  (warn "starting test-node")
  (let ((n nil))
    (warn "having n nil assign new instance ~s" n)
    (assign n (make-instance 'node) )
    (warn "we have n ~s" n)

    (warn "ZZZZZZZZZZZZZZZZZZZZZZZZZZZZz")
    (assign n 123)
    (warn "we have n ~s" n)

    (assign n (make-instance 'node) )
    (warn "we have n ~s" n)

    (assign n (make-instance 'node) )

    (assign (@ (ids n) 2) nil)
    (assign (@ (ids n) 1) nil)

    (warn "finally we have ~S" n)

    ;; was that simpler?
    (progn
      (let ((nid (id n)))
        (remhash nid (ids n))
        (setf n nil)))

    ;; or just
    ;; (destroy-object n)
    ;; (setf n nil)

    ;; (reset-everything n)
    n))


#| running tests
  (test-experiment)
  (test-mouse-movement)
  (test-node)
  |#
