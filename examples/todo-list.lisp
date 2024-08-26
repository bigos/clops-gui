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

(defun make-coordinates (x y)
    (make-instance 'gui-box:coordinates :x x :y y))

;;; === classes =================================================================
(defclass/std todo-window (gui-window:lisp-window)
  ((events)))

(defclass/std todo-box   (gui-box:box)
  ((items :std 0)
   (last-clicked)))
(defclass/std todo-item   (gui-box:text-box) (()))
(defclass/std action-box (gui-box:box) (()))

(defclass/std action-add (gui-box:text-box)  (()))
(defclass/std action-remove (gui-box:text-box) (()))

;;; === methods =================================================================
(defmethod initialize-instance :after ((window todo-window) &rest initargs &key)
  (declare (ignore initargs))

  (let
      ((todo-box   (make-instance 'todo-box
                                  :parent window
                                  :top-left (make-coordinates 20 100)
                                  :width 400
                                  :height 200))
       (action-box (make-instance 'action-box
                                  :parent window
                                  :top-left (make-coordinates 20 330)
                                  :width 400
                                  :height 50)))

    (gui-window:add-child window todo-box)

    (gui-window:add-child window action-box )

    (gui-box:add-child action-box (make-instance 'action-add
                                                 :parent action-box
                                                 :top-left (make-coordinates 5 5)
                                                 :width 20
                                                 :height 20
                                                 :text "Add"))
    (gui-box:add-child action-box (make-instance 'action-remove
                                                 :parent action-box
                                                 :top-left (make-coordinates 105 5)
                                                 :width 20
                                                 :height 20
                                                 :text "Remove"))))

(defmethod add-item ((window todo-window) string)
  (let ((todo-box (typed-widget window 'todo-box)))
    (gui-box:add-child todo-box (make-instance 'todo-item
                                               :parent todo-box
                                               :top-left (make-coordinates 4
                                                                           (+ (* (items todo-box) 40) 5) )
                                               :width 200
                                               :height 30
                                               :text (format nil "~a ~a"
                                                             string
                                                             (items todo-box))))
    (incf (items todo-box))))

(defmethod remove-item ((window todo-window))
  (let* ((todo-box (typed-widget window 'todo-box))
         (last-clicked (~> todo-box last-clicked)))
    (if last-clicked
      (progn
        (gui-box:remove-child todo-box last-clicked)
        (warn "removed last clicked")
        (when (> (items todo-box) 0)
          (decf (items todo-box)))
        ;; recalculate coordinates of todobox children
        (loop for c in (sort (gui-box:children todo-box)
                             (lambda (a b)
                               (string<= (gui-box:text a)
                                         (gui-box:text b))))
              for z = 0 then (1+ z)
              do
                 (gui-box:move-to c
                                  4
                                  (+ (* z 40) 5))))
      (warn "no last clicked detected"))))

(defmethod process-box ((window todo-window) (box T))
  (warn "processing box")
  (typecase box
    (action-add
     (warn "processing action-add")
     (add-item window "Item 1"))
    (action-remove
     (warn "processing acion-remove")
     (remove-item window))
    (todo-item
     (warn "processing todo-item click ~S" box)
     (setf (~> box gui-box:parent last-clicked) box)
     )
    (t
     (warn "going to process unknown box ~S" (type-of box)))))

(defun text-dimentions (text size font slant weight)
  (cairo:select-font-face font slant weight)
  (cairo:set-font-size size)

  (multiple-value-bind (xb yb width height)
      (cairo:text-extents text)
    (declare (ignore xb yb))
    (cons width height)))

(defgeneric render (box)
    (:documentation "Render a BOX usinng cairo "))

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

  (let ((my-text (format nil "~A" (~> box gui-box:text))))
    (let ((the-text-dimentions (text-dimentions my-text 20 "Ubuntu Mono" :normal :bold)))
      (let ((width  (car the-text-dimentions))
            (height (cdr the-text-dimentions)))

        ;; some text boxes will not be resized on text change
        (gui-box:resize box width height)

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
                       (~> box gui-box:top-left gui-box:absolute-y (+ _ height 2)))
        (gui-window:set-rgba "black")
        (cairo:show-text my-text)))))

;;; === experiment ==============================================================
(defun experiment-first-window ()
  (setf gui-window:*client-fn-draw-objects*  'todo-list::draw-window)

  (setf gui-window:*lisp-app* (gui-window::make-lisp-app))
  (assert (zerop (hash-table-count (gui-window:all-windows))))

  (let ((lisp-window (make-instance 'todo-window)))
    (gui-window:window-creation-from-simulation :testing lisp-window)
    (assert (eq 1 (hash-table-count (gui-window:all-windows))))
    lisp-window))

;;; when you skip the arguments part of the function this may work as a forward
;;: declaration for process-event and hide the warning
(declaim (ftype function process-event))

(defun experiment ()
  "Experiment is for experimenting with processing events and drawing windows
  without the use of Gtk4 library. Instead of drawing on a window the outcomes
  of the events are drawn on *.png files."
  (warn "starting experiment")

  (let ((lisp-window (experiment-first-window))
        (events '((:RESIZE ((600 400))) (:KEY-RELEASED (("" "Return" 36 NIL)))
                  (:TIMEOUT (NIL)) (:MOTION-ENTER ((194.0d0 390.0d0)))
                  (:MOTION ((194.81414794921875d0 390.444091796875d0)))
                  (:MOTION-LEAVE (NIL)) (:MOTION-ENTER ((0.0d0 332.0d0)))
                  (:MOTION ((0.110321044921875d0 332.4322509765625d0))) (:TIMEOUT (NIL))
                  (:MOTION ((39.44886779785156d0 346.9728088378906d0)))
                  (:PRESSED ((1 39.44886779785156d0 346.9728088378906d0)))
                  (:RELEASED ((1 39.44886779785156d0 346.9728088378906d0)))
                  (:TIMEOUT (NIL)) (:KEY-PRESSED (("e" "e" 26 NIL))))))
    (loop for event in events
          for e = (car event)
          for eargs = (caadr event)
          do
             (break "data ~s" (list
                               gui-window:*lisp-app*
                               lisp-window))
             (funcall 'process-event
                      lisp-window
                      e
                      eargs ))))

;;; (experiment2)
(defun experiment2 ()
  "Experiment is for experimenting with processing events and drawing windows
  without the use of Gtk4 library. Instead of drawing on a window the outcomes
  of the events are drawn on *.png files."
  (warn "starting experiment")

  (let* ((lisp-window (experiment-first-window))
         (events `((:RESIZE ((600 400)))
                   (:KEY-RELEASED (("" "Return" 36 NIL)))
                   (:MOTION-ENTER ((594.0d0 218.0d0)))
                   (:assert (eq 1 (hash-table-count (gui-window:all-windows))))
                   (:assert (typep (gethash :testing  (gui-window:all-windows)) 'todo-window))
                   (:assert (null (~> (gui-window:all-windows)
                                   (gethash :testing _)
                                   (gui-window:children _)
                                   (nth 1 _)
                                   (gui-box:children _))))
                   (:MOTION ((36.661827087402344d0 338.5277404785156d0)))
                   (:MOTION ((38.44976806640625d0 341.4234924316406d0)))
                   (:assert (equalp "Add" (~> (gui-window:all-windows)
                                           (gethash :testing _)
                                           (gui-window:most-current-widget _)
                                           (gui-box:text _))))
                   (:PRESSED ((1 38.44976806640625d0 341.4234924316406d0)))
                   (:assert (eq 1  (~> (gui-window:all-windows)
                                    (gethash :testing _)
                                    (gui-window:children _)
                                    (nth 1 _)
                                    (gui-box:children _)
                                    (length _) )))
                   (:RELEASED ((1 38.44976806640625d0 341.4234924316406d0)))
                   (:PRESSED ((1 38.44976806640625d0 341.4234924316406d0)))
                   (:assert (eq 2  (~> (gui-window:all-windows)
                                    (gethash :testing _)
                                    (gui-window:children _)
                                    (nth 1 _)
                                    (gui-box:children _)
                                    (length _) )))
                   (:RELEASED ((1 38.44976806640625d0 341.4234924316406d0)))
                   (:PRESSED ((1 38.44976806640625d0 341.4234924316406d0)))
                   (:assert (eq 3  (~> (gui-window:all-windows)
                                    (gethash :testing _)
                                    (gui-window:children _)
                                    (nth 1 _)
                                    (gui-box:children _)
                                    (length _) )))
                   (:RELEASED ((1 38.44976806640625d0 341.4234924316406d0)))
                   (:MOTION ((95.17391967773438d0 152.87742614746094d0)))
                   (:MOTION ((82.94773864746094d0 148.03538513183594d0)))
                   (:PRESSED ((1 82.94773864746094d0 148.03538513183594d0)))
                   (:assert (eq 3  (~> (gui-window:all-windows)
                                    (gethash :testing _)
                                    (gui-window:children _)
                                    (nth 1 _)
                                    (gui-box:children _)
                                    (length _) )))
                   (:RELEASED ((1 82.94773864746094d0 148.03538513183594d0)))
                   ;;(:MOTION ((169.45411682128906d0 341.844970703125d0)))
                   ;; try to find the center coordinates
                   (:motion (,(loop for hv being the hash-value in
                                                                (gui-window:all-widgets
                                                                 (gethash :testing
                                                                          (gui-window:all-windows)))
                                    until (typep hv 'action-remove) finally
                                      (return (gui-box:central-point hv)))))

                   (:assert (equal "Remove" (~> (gui-window:all-windows)
                                             (gethash :testing _)
                                             (gui-window:most-current-widget _)
                                             (gui-box:text _))))
                   (:PRESSED ((1 169.45411682128906d0 341.844970703125d0)))
                   (:assert (eq 2  (~> (gui-window:all-windows)
                                    (gethash :testing _)
                                    (gui-window:children _)
                                    (nth 1 _)
                                    (gui-box:children _)
                                    (length _) )))
                   (:RELEASED ((1 169.45411682128906d0 341.844970703125d0)))
                   (:MOTION ((533.4215698242188d0 60.727294921875d0)))
                   (:KEY-PRESSED (("e" "e" 26 NIL))))))
    (loop for event in events
          for e = (car event)
          for eargs = (caadr event)
          do
             (if (eq e :assert)
                 (assert (eval (cadr event)))
                 (funcall 'process-event
                          lisp-window
                          e
                          eargs )))))

;;; ============================================================================
(defun typed-widget (window widget-type)
  (first
   (loop for w in (gui-window:children window) when (typep w widget-type) collect w)))

(defmethod draw-window ((window todo-window))
  "Calls render for topmost boxes of the window."
  (let ((cv 0.13)) (cairo:set-source-rgb  cv cv cv))
  (cairo:paint)

  (render (typed-widget window 'todo-box))
  (render (typed-widget window 'action-box))

  (let ((app gui-window:*lisp-app*))
    (when (and (eq (gui-window:current-motion app)
                   window)
               (gui-window:mouse-coordinates app))
      (gui-window:set-rgba "pink")
      (cairo:rectangle
       (car (gui-window:mouse-coordinates app))
       (cdr (gui-window:mouse-coordinates app))
       25
       25)
      (cairo:fill-path))))

(defun store-events (lisp-window event args)
  (if (and (eq event :motion)
           (eq (car (first (events lisp-window))) :motion))
      (setf (first (events lisp-window)) (list event args))
      (push (list event args) (events lisp-window))))

(defmethod process-event ((lisp-window todo-window) event &rest args)
  (store-events lisp-window event args)

  (case event
    (:timeout
     ;; do nothing yet
     )
    ((:motion :motion-enter) ; we use simple case with one window so we ignore the window argument
     (destructuring-bind ((x y)) args
       (gui-window:mouse-motion-enter lisp-window x y)))
    (:motion-leave
     (gui-window:mouse-motion-leave))
    (:focus-enter)
    (:focus-leave)
    (:pressed
     (destructuring-bind ((button x y)) args
       (declare (ignore button))
       (let ((current-widget (gui-window:most-current-widget lisp-window)))
         (format t "~&processing mouse at ~S ~S ~%on widget ~S~%" x y
                 current-widget)
         (gui-window:mouse-button-pressed current-widget)
         (process-box lisp-window current-widget))))

    (:released
     (gui-window:mouse-button-released))
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
         ((equal entered "e")
          (format t "events ~S" (reverse (events lisp-window))))
         (t nil))))
    (otherwise
     (warn "not handled event ~S ~S" event args)))

  ;; redrawing ------------------------------
  (gui-window:redraw-canvas lisp-window (format  nil "~A" event)))

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
