(declaim (optimize (speed 0) (safety 2) (debug 3)))

;; also roam for githubkasken
;; file:~/Documents/Roams/githubkasten/org-roam/20240808235036-order_of_development.org::8

;;;; Example of resizing-sections window

;;; load ===================================================================
;; (load "~/Programming/Lisp/clops-gui/examples/resizing-sections.lisp")
(push #p "~/Programming/Lisp/clops-gui/" ql:*local-project-directories*)
(ql:quickload '(:clops-gui) :silent T)

;;; package ================================================================
(defpackage #:resizing-sections
  (:use #:cl)
  (:import-from :serapeum
   :~>)
  (:import-from :defclass-std
   :defclass/std))

(in-package #:resizing-sections)
;;; musing =====================================================================

;; ** define classes
;; and think how they relate to each other

;; ** define interfaces
;; and think how clients can create and manipulate instances
;; think about defgenerics and *WHAT* operations they perform

;; ** define implementations
;; and think *HOW* to perform the operations

;; *** interface
;; is the middleman between two distinct components
;; defines how components communicate using requests and responses
;; allows to talk to abstraction of the other component

;; *** utilities and print-object

;; *** component
;; **** external vs internal
;; has external part exposed to the client, that should be stable
;; and internal (private)
;; The key is to find which parts should not be exposed to the external client
;; **** responsibilities
;; component may have specific responsibilities
;; interface defined the behaviour of the component from the standpoint of the client


;;; classes !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

;;; existing classes gui-app and gui-window
(defclass/std resizing-sections-window (gui-window:lisp-window) (()))

;; TODO add classes for new implementation of boxes

(defclass/std point ()
  ((x)
   (y)
   (absolute-x)
   (absolute-y)
   (parent)))

;; rect-base - base class for widgets
(defclass/std rect-base ()
  ((resizing-point :doc "point around which resizing will perform" :type point)
   ;; distances to the edge from the resizing point
   (up)
   (right)
   (down)
   (left)
   (children)))

;; rect-window - first widget in a window
;; belongs to gui-window
;; has meny rects
;; resizes only with the window, does not move
(defclass/std rect-window (rect-base)
  ((lisp-window :doc "gui-widow:lisp-window wrapping the GTK window" :type gui-window:lisp-window)))

;; rect - normal widget
;; belongs to rect-window or rect
;; different rects may have diffferent properties that decide how it is resized and
;; how children are made to resize, also handles wrapping and truncating of content
(defclass/std rect (rect-base)
  ())


;;; interfaces !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
(defgeneric to-rectangle (rect)
  (:documentation "convert rect to x,y,width, height used by cairo"))

;;; utilities !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

;; TODO add print-object and better inspector

(progn ;; slot names and printing object
    (defun slot-names (obj)
      (~> obj
          class-of
          sb-mop:class-slots
          (mapcar #'sb-mop:slot-definition-name  _)))

  (defun slot-names-and-classes (obj)
    (loop for the-slot in (slot-names obj)
          collect (if (slot-boundp obj the-slot)
                      (cons the-slot (type-of (slot-value obj the-slot)))
                      the-slot)))

  (defun slot-values-except (obj exceptions)
    (loop for the-slot in (slot-names obj)
          collect (if (slot-boundp obj the-slot)
                      (if (member the-slot exceptions)
                          (list the-slot
                                :ignored
                                (type-of (slot-value obj the-slot)))
                          (cons the-slot
                                (slot-value obj the-slot)))
                      the-slot)))

  (defmethod print-object ((object point) stream)
    (print-unreadable-object (object stream :identity t :type t)
      (format stream "~Sx~S"
              (x object)
              (y object))))

  (defmethod print-object ((object gui-box:coordinates) stream)
    (print-unreadable-object (object stream :identity t :type t)
      (format stream "rel: ~Sx~S, abs: ~Sx~S"
              (gui-box:x object)
              (gui-box:y object)
              (gui-box:absolute-x object)
              (gui-box:absolute-y object))))

  (defmethod print-object ((object gui-box:box) stream)
    (print-unreadable-object (object stream :identity t :type t)
      (format stream "obj: ~S"
              (slot-values-except object '(gui-box:parent
                                           gui-box:children)))))


  ;; (defmethod print-object ((object resizing-sections-window) stream)
  ;;   (print-unreadable-object (object stream :identity t :type t)
  ;;     (format stream "obj: ~S"
  ;;             (slot-values-except object '()))))

  ;; (defmethod print-object ((object rect-window) stream)
  ;;   (print-unreadable-object (object stream :identity t :type t)
  ;;     (format stream "obj: ~S"
  ;;             (slot-values-except object '()))))

  ;; end of progn
  )

;;; defmethods !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
(defmethod to-rectangle ((rect rect-base))
  (let ((rs (resizing-point rect)))
    (when (parent rs)
      (when (null (absolute-x rs))
        (setf (absolute-x rs) (+ (absolute-x (parent rs)) (x rs))))
      (when (null (absolute-y rs))
        (setf (absolute-y rs) (+ (absolute-y (parent rs)) (y rs)))))

    (let ((x (- (absolute-x rs)
                (left rect)))
          (y (- (absolute-y rs)
                (up rect)))
          (width (+ (right rect)
                    (left rect)))
          (height (+ (up rect)
                     (down rect))))
      (list x y width height))))

;;; components !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

;;; drawing is a component used by GTK to draw on canvas
;;; events is a component for responding to GTK events

;;; description of another component and its implementation will go here

;;; constructors ---------------------------------------------------------------
(defun make-point (x y)
    (make-instance 'point :x x
                          :y y))

(defun make-rect (rp up right down left)
  (make-instance 'rect
                 :resizing-point (make-point (car rp) (cdr rp))
                 :up up
                 :right right
                 :down down
                 :left left))

;;; zzzzzz ----------------------------------------------------------------
(defmethod adjust-absolute ((widget rect-base))

  (setf (absolute-x (resizing-point widget)) (+ (~> widget resizing-point parent absolute-x)
                                                      (~> widget resizing-point x)))
  (assert (~> widget resizing-point absolute-x) nil "just after")


  (setf (absolute-y (resizing-point widget)) (+ (~> widget resizing-point parent absolute-y)
                                                      (~> widget resizing-point y))))

(defmethod add-child ((lisp-window gui-window:lisp-window) (box rect-window))
  (if (null (gui-window:children lisp-window))
      (progn
        (setf (lisp-window box) lisp-window)
        (pushnew box (gui-window:children lisp-window)))
      (error "you can not add more than one widget to the window")))

(defmethod add-child :before ((parent-widget rect-base) (child-widget rect-base))
  ;; validate presence of required fields
  (unless (typep parent-widget 'rect-window)
    (assert (~> parent-widget resizing-point parent)))
  (assert (~> parent-widget resizing-point absolute-x))
  (assert (~> parent-widget resizing-point absolute-y)))

(defmethod add-child ((parent-widget rect-base) (child-widget rect-base))
  (setf (~> child-widget resizing-point parent) (~> parent-widget resizing-point))

  (adjust-absolute child-widget)
  (pushnew child-widget (children parent-widget)))

;;; TODO fix the assertion here
(defmethod add-child :after ((parent-widget rect-base) (child-widget rect-base))
  ;; validate presence of required fields
  (assert (~> child-widget resizing-point absolute-x) nil "failed in after method")
  (assert (~> child-widget resizing-point absolute-y)))

;; (defmethod add-child ((parent-widget rect) (child-widget rect))
;;   (setf (~> child-widget resizing-point parent) (~> parent-widget resizing-point))
;;   (pushnew child-widget (children parent-widget)))

(defmethod initialize-instance :after ((window resizing-sections-window) &rest initargs &key)
  (declare (ignore initargs))
  ;; add child
  (let ((window-widget (make-instance 'rect-window
                                      :resizing-point (make-instance 'point
                                                                     :x 0
                                                                     :y 0
                                                                     :parent nil
                                                                     :absolute-x 0
                                                                     :absolute-y 0)
                                      :up 0
                                      :right nil ;we do not have window yet
                                      :down  nil
                                      :left 0)))
    (add-child window window-widget)
    (let ((widget-a1   (make-rect '(0 .   10) 0 50 50 0))
          (widget-a2   (make-rect '(150 . 60) 30 30 30 30))
          (widget-a3   (make-rect '(300 . 10) 0 0 50 50))
          (widget-a2b1 (make-rect '(110 . 20) 0 10 10 0))
          (widget-a2b2 (make-rect '(130 . 30) 0 10 10 0)))
      (add-child window-widget widget-a1)
      (add-child window-widget widget-a2)
      (add-child window-widget widget-a3)

      (add-child widget-a2 widget-a2b1)
      (add-child widget-a2 widget-a2b2))
    (warn "added widgets, final inspect")
  ;  (swank:inspect-in-emacs window-widget :wait T)
    ))
(defmethod window-resize  :before (w h (window resizing-sections-window))
  (let ((window-widget (car (gui-window:children window))))
    (assert (typep window-widget 'rect-window))
    (assert (zerop (up window-widget)))
    (assert (zerop (left window-widget)))))

(defmethod window-resize  (w h (window resizing-sections-window))
  (warn "resizing ~S" (class-of window))
  (let ((window-widget (car (gui-window:children window))))
    (setf (right window-widget) w)
    (setf (down window-widget) h)
    (if t
        (describe window-widget))))

(defmethod move-top-widgets ((window gui-window:lisp-window))
  (let ((ww (first (gui-window:children window))))
    (let ((ax (~> ww children)))
      (let ((al (elt ax 2))
            (ac (elt ax 1))
            (ar (elt ax 0))
            (window-center (/ (right ww) 2))
            (window-right  (right ww)))

        (setf (~> al resizing-point x) 10)
        (setf (~> al resizing-point y) 10)
        (adjust-absolute al)
        (setf (~> ac resizing-point x) window-center )
        (setf (~> ac resizing-point y) 60)
        (adjust-absolute ac)
        (setf (~> ar resizing-point x) (- window-right 10))
        (setf (~> ar resizing-point y) 10)
        (adjust-absolute ar)
        ))))

;;; rendering ==================================================================
(defmethod render ((widget rect))
  (warn "zzz ~s" (class-of widget))

  (gui-color:set-rgba
   (if (>= 30 (right widget))
       "#00BB00A0"
       "#aaaa00A0"))
  ;; figure out to draw childrent at relative coordinates
  (let ((rd (to-rectangle widget)))
    (apply 'cairo:rectangle rd)
    (cairo:fill-path))

  (gui-window:set-rgba "red")
  (cairo:arc (x (resizing-point widget))
             (y (resizing-point widget))
             3
             0 (* 2 pi))

  (cairo:fill-path)

  (call-next-method))

(defmethod render ((widget T))
  (warn "going to render ~s" (class-of widget))

  (loop for c in (children widget)
        do (render c)))

;;; drawing ====================================================================

(defun draw-mouse-square (window)
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

(defun draw-text-1 (window)
  (declare (ignore window))
  (cairo:select-font-face "Ubuntu Mono" :italic :bold)
  (cairo:set-font-size 10)
  (cairo:move-to 10 10)

  (gui-window:set-rgba "black")

  (cairo:show-text (format nil "Try to resize the window and see how the elements respond to resizing")))

(defun draw-text-2 (window)
  (cairo:select-font-face "Ubuntu Mono" :italic :bold)
  (cairo:set-font-size 15)
  (cairo:move-to 10 100)

  (let ((cmotion    (gui-app:current-motion-window-p gui-app:*lisp-app* window)))
    (if cmotion
        (gui-window:set-rgba "green")
        (gui-window:set-rgba "red"))

    (let ((ff (first (gui-window:children window))))

      (cairo:show-text (format nil "dimensions ~s" (list (right ff) (down ff))
                               )))))

;; In main function we tell to use draw-window to draw on canvas
(defmethod draw-window ((window resizing-sections-window))
  ;; paint background
  (let ((cv 0.95)) (cairo:set-source-rgb  cv cv cv))
  (cairo:paint)

  (draw-text-1 window)
  (draw-text-2 window)

  ;; render window children
  (loop for c in (gui-window:children window)
        do (render c))

  (draw-mouse-square window))

;;; events =====================================================================
;;; in main function we tell to use process-event to respond to GTK events
(defmethod process-event ((lisp-window resizing-sections-window) event &rest args)
  (unless (eq event :timeout)
    (warn "event ~S ~S" event args))

  (case event
    (:timeout
     ;; do nothing yet
     )
    ((:motion :motion-enter)
     ;; we use simple case with one window so we ignore the window argument
     (destructuring-bind ((x y)) args
       (gui-app:mouse-motion-enter lisp-window x y)))
    (:motion-leave
     (gui-app:mouse-motion-leave))
    (:focus-enter)
    (:focus-leave)
    (:pressed
     (destructuring-bind ((button x y)) args
       (declare (ignore button x y))
       ))
    (:released
     (gui-app:mouse-button-released))
    (:scroll)
    (:resize
     (destructuring-bind ((w h)) args
       (window-resize w h lisp-window)
       (move-top-widgets lisp-window)))
    (:key-pressed
     (destructuring-bind ((entered key-name key-code mods)) args
       (format t "~&>>> key pressed ~S~%" (list entered key-name key-code mods))
       ))
    (otherwise
     (warn "not handled event ~S ~S" event args)))

  ;; redrawing ------------------------------
  (gui-window:redraw-canvas lisp-window (format  nil "~A" event)))

;;; main =======================================================================
(defun main ()
  (setf
   gui-drawing:*client-fn-draw-objects*  'resizing-sections::draw-window
   gui-window-gtk:*client-fn-menu-bar*      nil
   gui-events:*client-fn-process-event* 'resizing-sections::process-event
   gui-window-gtk:*initial-window-width*    600
   gui-window-gtk:*initial-window-height*   400
   gui-window-gtk:*initial-title*           "Resizing Sections")

  (gui-window-gtk:window (make-instance 'resizing-sections-window)))

;; (main)
