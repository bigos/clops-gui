(declaim (optimize (speed 0) (safety 2) (debug 3)))

;; (load "~/Programming/Lisp/clops-gui/examples/todo-list.lisp")

;;; === load ===================================================================
(push #p "~/Programming/Lisp/clops-gui/" ql:*local-project-directories*)
(ql:quickload '(:clops-gui) :silent nil)

;;; === package ================================================================
(defpackage #:todo-list
  (:use #:cl)
  (:import-from :serapeum
                :~>)
  (:import-from :defclass-std
                :defclass/std))

(in-package #:todo-list)

;;; === utilities ==============================================================
(defun slot-values (obj)
  (loop for the-slot in (mapcar
                         #'sb-mop:slot-definition-name
                         (sb-mop:class-slots (class-of obj)))
        collect (if (slot-boundp obj the-slot)
                    (cons the-slot
                          (slot-value obj the-slot))
                    the-slot)))

(defmethod print-object ((object gui-box:coordinates) stream)
  (print-unreadable-object (object stream :identity t :type t)
    (format stream "rel: ~S, abs: ~S"
            (list (gui-box:x object) (gui-box:y object))
            (list (gui-box:absolute-x object) (gui-box:absolute-y object)))))

;; (defmethod print-object ((object gui-box:box) stream)
;;   (print-unreadable-object (object stream :identity t :type t)
;;     (format stream "obj: ~S"
;;             (slot-values object))))

(defun make-coordinates (x y)
  (make-instance 'gui-box:coordinates :x x :y y))

(defmethod box-coordinates ((box gui-box:box) x y)
  ;; we need to consider parents in relative coordinates
  (loop for b in (gui-box:parent-boxes box)
        collect (cons (~> b gui-box:top-left gui-box:x)
                      (~> b gui-box:top-left gui-box:y))))

;;; === classes =================================================================
(defclass/std todo-window (gui-window:lisp-window)
  ((search-box)
   (todo-box)
   (action-box)))

(defclass/std search-box (gui-box:box)
  ((text-field)
   (button)
   (inner-button)))

(defclass/std todo-box   (gui-box:box) (()))
(defclass/std action-box (gui-box:box) (()))

(defclass/std action-add (gui-box:text-box)  (()))
(defclass/std action-remove (gui-box:text-box) (()))
(defclass/std action-up (gui-box:text-box)   (()))
(defclass/std action-down (gui-box:text-box) (()))

;;; === methods =================================================================
(defmethod initialize-instance :after ((window todo-window) &rest initargs &key)
  (declare (ignore initargs))

  ;; TODO think about relative coordinates

  ;; because these coordinates are in lisp-window they are absolute

  (setf
   (search-box window) (make-instance 'search-box
                                      :parent window
                                      :top-left (make-coordinates 20 20)
                                      :width 400
                                      :height 50)
   (todo-box window)   (make-instance 'todo-box
                                      :parent window
                                      :top-left (make-coordinates 20 100)
                                      :width 400
                                      :height 200)
   (action-box window) (make-instance 'action-box
                                      :parent window
                                      :top-left (make-coordinates 20 330)
                                      :width 400
                                      :height 50))

  (gui-window:add-child window (search-box window))
  (gui-window:add-child window (todo-box   window))
  (gui-window:add-child window (action-box window))

  (gui-box:add-child (~> window action-box) (make-instance 'action-add
                                                           :parent (~> window action-box)
                                                           :top-left (make-coordinates 5 5)
                                                           :width 20
                                                           :height 20
                                                           :text "Add"))

  (gui-box:add-child (~> window action-box) (make-instance 'action-remove
                                                           :parent (~> window action-box)
                                                           :top-left (make-coordinates 105 5)
                                                           :width 20
                                                           :height 20
                                                           :text "Remove"))

  (gui-box:add-child (~> window action-box) (make-instance 'action-up
                                                           :parent (~> window action-box)
                                                           :top-left (make-coordinates 205 5)
                                                           :width 20
                                                           :height 20
                                                           :text "Up"))

  (gui-box:add-child (~> window action-box) (make-instance 'action-down
                                                           :parent (~> window action-box)
                                                           :top-left (make-coordinates 305 5)
                                                           :width 20
                                                           :height 20
                                                           :text "Down")))

(defmethod initialize-instance :after ((box search-box) &rest initargs &key)
  (declare (ignore initargs))

  ;; TODO think about relative coordinates
  ;; TODO step here and analyse the possibilities

  ;; there we should add the parent coordinates
  ;; and have 20+20 20+10 instead of 40 30
  ;; the addition should be done at rendering stage
  (setf
   (text-field box) (make-instance 'gui-box:text-box
                                   :parent box
                                   :top-left (make-coordinates 10 10)
                                   :width 200
                                   :height 30)
   (button box)     (make-instance 'gui-box:text-box
                                   :parent box
                                   :top-left (make-coordinates 280 10)
                                   :width 110
                                   :height 30
                                   :text "Search"))

  (gui-box:add-child box (text-field box))
  (gui-box:add-child box (button box)))

(defmethod initialize-instance :after ((box gui-box:text-box) &rest initargs &key)
  (declare (ignore initargs))
  (setf (gui-box::recalculate box) t))

(defun text-dimentions (text &optional (size 20) (font "Ubuntu Mono"))
  (cairo:select-font-face text :normal :bold)
  (cairo:set-font-size size)

  (multiple-value-bind (xb yb width height)
      (cairo:text-extents text)
    (declare (ignore xb yb))
    (list width height)))

(defgeneric render (box)
    (:documentation "Render a BOX usinng cairo "))

(defmethod render ((search-box search-box))
  (cairo:select-font-face "Ubuntu Mono" :normal :bold)
  (cairo:set-font-size 12)

  (let ((my-text (format nil "~A" "search box")))
    (multiple-value-bind (xb yb width height)
        (cairo:text-extents "X")
      (declare (ignore xb yb width))

      (cairo:move-to    (~> search-box gui-box:top-left gui-box:absolute-x)
                        (+ (~> search-box gui-box:top-left gui-box:absolute-y) height)))
    (gui-window:set-rgba "red")
    (cairo:show-text my-text))

  (gui-window:set-rgba "#55667740")
  (cairo:rectangle
   (~> search-box gui-box:top-left gui-box:absolute-x)
   (~> search-box gui-box:top-left gui-box:absolute-y)
   (~> search-box gui-box:width)
   (~> search-box gui-box:height))
  (cairo:fill-path))

(defmethod render ((box gui-box:box))
  (gui-window:set-rgba "#ffff0040")
  (cairo:rectangle
   (~> box gui-box:top-left gui-box:absolute-x)
   (~> box gui-box:top-left gui-box:absolute-y)
   (~> box gui-box:width)
   (~> box gui-box:height))
  (cairo:fill-path))

(defmethod render :after ((box gui-box:box) )
  (loop for b in (gui-box:children box) do (render b)))

(defmethod render ((box gui-box:text-box))
  (gui-window:set-rgba (gui-box:box-color box))

  (cairo:select-font-face "Ubuntu Mono" :normal :bold)
  (cairo:set-font-size 20)

  (warn "text dimensions calculation ~S" (text-dimentions "abc"))

  (let ((my-text (format nil "~A" (~> box gui-box:text))))
    ;; get the text width and height
    (multiple-value-bind (xb yb width height)
        (cairo:text-extents my-text)
      (declare (ignore xb yb))

      (progn
        ;; set the box relative dimensions and recalculate the absolute dimensions
        (setf (~> box gui-box:width) (+ width 4)
              (~> box gui-box:height) (+ height 4))
        (when (gui-box::recalculate box)
          (gui-box:recalculate-absolute box)
          (setf (gui-box::recalculate box) nil)))

      ;; draw a box with 4/2 pixels margin
      (cairo:rectangle
       (~> box gui-box:top-left gui-box:absolute-x)
       (~> box gui-box:top-left gui-box:absolute-y)
       (+ width 4)
       (+ height 4))
      (cairo:fill-path)

      ;; move cairo cursor relative to absolute box position to have the margin
      (cairo:move-to (~> box gui-box:top-left gui-box:absolute-x (+ _  2))
                     ;; so the height is useless here because I can not line up the -
                     (~> box gui-box:top-left gui-box:absolute-y (+ _ height 2))))
    (gui-window:set-rgba "black")
    (cairo:show-text my-text)))

;;; === experiment ==============================================================
(defun experiment-first-window ()
      (setf
       gui-window:*client-fn-draw-objects*  'todo-list::draw-window
       gui-window:*lisp-app* (make-instance 'gui-window::lisp-app))

  (assert (zerop (hash-table-count (gui-window:all-windows))))

  (let ((lisp-window (make-instance 'todo-window)))
    (gui-window:window-creation-from-simulation :testing lisp-window)
    (assert (eq 1 (hash-table-count (gui-window:all-windows))))
    lisp-window))

(defun experiment ()
  "Experiment is for experimenting with processing events and drawing windows
  without the use of Gtk4 linrary. Instead of drawing on a window the outcomes
  of the events are drawn on *.png files."
  (warn "starting experiment")

  (let ((lisp-window (experiment-first-window)))
    (process-event lisp-window :resize (list 800 400 ))
    (process-event lisp-window :resize (list 600 200 ))
    (break "checking the widget structure ~S" (list gui-window:*lisp-app*
                                                    lisp-window))
    (process-event lisp-window :timeout)
    (process-event lisp-window :motion-enter (list 50 50 ))

    (describe gui-window:*lisp-app* )    ;; it works


    ;; (process-event lisp-window :key-pressed (list "" "Escape" 9  nil))
    ;; (process-event lisp-window :key-pressed (list "" "F1" 67     nil))
    ;; (process-event lisp-window :key-pressed (list "r" "r" 10     nil))
    ;; (process-event lisp-window :key-pressed (list " " "space" 65 nil))
    ))

;;; ============================================================================
(defmethod draw-window ((window todo-window))
  "Calls render for topmost boxes of the window."
  (let ((cv 0.13)) (cairo:set-source-rgb  cv cv cv))
  (cairo:paint)

  (cairo:select-font-face "Ubuntu Mono" :normal :bold)
  (cairo:set-font-size 20)

  (let ((my-text (format nil "~A" "Let's go!")))
    (multiple-value-bind (xb yb width height)
        (cairo:text-extents "X")
      (declare (ignore xb yb width height))

      (cairo:move-to 20 20))
    (gui-window:set-rgba "black")
    (cairo:show-text my-text))

  (render (search-box window))
  (render (todo-box window))
  (render (action-box window)))

(defmethod process-event ((lisp-window todo-window) event &rest args)
  (unless (member event '(:timeout :motion))
    (format t "~&going to process ~A ~A  "
            event
            (case event
              ((:focus-enter :focus-leave)
               (gui-window:window-hkey lisp-window))
              (:key-pressed
               (destructuring-bind ((letter name code mods)) args
                 (warn "pressed ~S" (list letter name code mods (gui-window:window-hkey lisp-window)))))
              (T args))))
  (case event
    (:timeout
     ;; do nothing yet
     )
    ((:motion :motion-enter) ; we use simple case with one window so we ignore the window argument
     (destructuring-bind ((x y)) args
       (setf (gui-window:mouse-coordinates gui-window:*lisp-app*) (cons x y)
             (gui-window:current-motion    gui-window:*lisp-app*) lisp-window)))
    (:motion-leave
     (setf (gui-window:mouse-coordinates gui-window:*lisp-app*) nil
           (gui-window:current-motion gui-window:*lisp-app*) nil))
    (:focus-enter)
    (:focus-leave)
    (:pressed
     (destructuring-bind ((button x y)) args
       (declare (ignore button))
       (format t "~&processing mouse at ~S ~S on widget ~S~%" x y
               (gui-window:most-current-widget lisp-window))))

    (:released)
    (:scroll)
    (:resize
     (destructuring-bind ((w h)) args
       (gui-window:window-resize w h lisp-window)))
    (:key-pressed
     (destructuring-bind ((entered key-name key-code mods)) args
       (format t "~&>>> key pressed ~S~%" (list entered key-name key-code mods))
       (cond
         ((equal entered "p")
          (break "break to examine ~S" lisp-window))
         (t nil))))
    (otherwise
     (warn "not handled event ~S ~S" event args)))

  ;; add drawing ------------------------------

  ;; (maphash (lambda (key lwin)
  ;;            (declare (ignore key))
  ;;            (gui-window:redraw-canvas lwin))
  ;;          (gui-window:all-windows))

  (gui-window:redraw-canvas lisp-window))

;;; === main ===================================================================
(defun init ()
  (setf
   ;; *model*  (make-instance 'model)
   ))

(defun main ()
  (init)

  (setf gui-window:*client-fn-menu-bar*      nil
        gui-window:*client-fn-draw-objects*  'todo-list::draw-window
        gui-events:*client-fn-process-event* 'todo-list::process-event
        gui-window:*initial-window-width*    600
        gui-window:*initial-window-height*   400
        gui-window:*initial-title*           "To-Do List")

  (gui-window:window (make-instance 'todo-window)))

;; (main)
