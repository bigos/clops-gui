(declaim (optimize (speed 0) (safety 3) (debug 3)))

;; (load "~/Programming/Lisp/clops-gui/examples/counter-third.lisp")
(push #p "~/Programming/Lisp/clops-gui/" ql:*local-project-directories*)
(ql:quickload '(:clops-gui
                :serapeum
                :defclass-std
                :cl-containers)
              :silent nil)

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

;;; --------------------------- globals ----------------------------------------
(defparameter *model-ids* nil)
(setf *model-ids* (make-hash-table))

(defparameter *model* (list
                       :size nil
                       :mouse-position nil
                       :counted 0
                       ))

(defparameter *model-id* nil)


;;; -------------------------------- code --------------------------------------
(cffi:defcstruct gdk-rgba
    (red   :float)
  (green :float)
  (blue  :float)
  (alpha :float))

(progn ;; classes
  (defclass/std counter-third-window (gui-window:lisp-window)
    (()))

  (defclass/std node ()
    ((id)
     (attrs)
     (parent-id)
     (children-ids)))

  (defclass/std root   (node) (()))
  (defclass/std box    (node) (()))
  (defclass/std button (node) (()))
  (defclass/std text   (node) (())))

(defmethod initialize-instance :after ((node node) &key)
  (assert (null (gethash (id node) *model-ids*)) nil "ID ~s is already taken" (id node))
  (assert (not (equal (id node) (parent-id node))))

  ;; add node to ids
  (setf (gethash (id node) *model-ids*) node)

  ;; add to children ids of the parent
  (when (and (parent-id node)
             (gethash (parent-id node) *model-ids*))
    (pushnew (id node)
             (children-ids (gethash (parent-id node)
                                    *model-ids*))))
  (warn "initialized ~s" (list node (id node) (parent-id node))))

(progn   ;; utilities
  (defmethod print-object ((obj node) stream)
      (print-unreadable-object (obj stream :type t :identity t)
        (format stream "~a"
                (loop for sl in (sb-mop:class-slots (class-of obj))
                      for slot-name = (sb-mop:slot-definition-name sl)
                      collect (cons slot-name
                                    (if (slot-boundp obj slot-name)
                                        (format nil "~S" (slot-value obj slot-name))))))))

  (defun build-nodes (parent tree)
    (destructuring-bind  (tag attrs &optional children) tree
      (let ((nt (make-instance tag
                               :id (incf *model-id*)
                               :parent-id parent
                               :attrs attrs)))
        (mapcar (lambda (e)
                  (build-nodes (id nt) e))
                children))))

  (defun set-rgba (color)
    (labels ((color-to-rgba (color)
               (cffi:with-foreign-object (rgba '(:struct gdk-rgba))
                 (let ((pointer (make-instance 'gir::struct-instance
                                               :class (gir:nget gdk::*ns* "RGBA")
                                               :this rgba)))
                   (let ((valid-color (gdk:rgba-parse pointer color)))
                     (cffi:with-foreign-slots ((red green blue alpha) rgba (:struct gdk-rgba))
                       (list valid-color red green blue alpha)))))))
      (let ((parsed-color (color-to-rgba color)))
        (if (first parsed-color)
            (apply 'cairo:set-source-rgba (rest parsed-color))
            (error "~S is not a valid color" color)))))

  )




(defun render-mouse (app)
  (let* ((mouse-position (gui-app:mouse-coordinates app))
         (mx (car mouse-position))
         (my (cdr mouse-position))
         (px (+ mx 20))
         (py (+ my 20))
         (po 6)
         (ao 15))
    (when mouse-position
      (labels ((drrr ()
                 (cairo:line-to (+ 50 mx) (+ po 50 my)) ; end
                 (cairo:line-to px (+ po py))
                 (cairo:line-to (+ ao mx) (+ 50 my))
                 (cairo:line-to mx my)  ;mp
                 (cairo:line-to (+ 50 mx) (+ ao my))
                 (cairo:line-to (+ po px) py)
                 (cairo:line-to (+ po 50 mx) (+ 50 my)) ; end
                 (cairo:line-to (+ 50 mx) (+ po 50 my)) ; end
                 ))
        (drrr)
        (cairo:set-source-rgba 0.2 1.0 0.3 0.3)
        (cairo:fill-path)

        (cairo:set-line-width 1.0)
        (drrr)
        (cairo:set-source-rgb 0.0 0.0 0.0) ; http://davidbau.com/colors/
        (cairo:stroke)))))

(defmethod draw-window ((window counter-third-window))
  ;; paint background
  (let ((cv 0.95)) (cairo:set-source-rgb  cv cv cv))
  (cairo:paint)

  (cairo:select-font-face "Ubuntu Mono" :italic :bold)
  (cairo:set-font-size 20)
  (cairo:move-to 20 30)
  (gui-window:set-rgba "black")
  (cairo:show-text (format nil "try to code something"))

  (loop for v being the hash-value in *model-ids*
        when (null (parent-id v)) do (render v)))

(defmethod draw-window :after ((window counter-third-window))
  ;; pink square follows the mouse
  (let ((app gui-app:*lisp-app*))
    (when (and (eq (gui-app:current-motion app)
                   window)
               (gui-app:mouse-coordinates app))
      (render-mouse app))))

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
       (gui-app:mouse-motion-enter lisp-window x y)
       (setf (getf *model* :mouse-position) (cons x y))))
    (:motion-leave
     (gui-app:mouse-motion-leave)
     (setf (getf *model* :mouse-position) nil))
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
       (gui-window:window-resize w h lisp-window)
       (setf (getf *model* :size) (cons w h))))
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
(defun init-model ()
  (when T
    (progn
      (clrhash *model-ids*)
      (setf *model-id* 10)
      (build-nodes nil
                   '(root nil
                     ((box nil
                       ((button (:label "+"))
                        (text nil)
                        (button (:label "-"))))))))))

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
    (process-gtk-event win :RELEASED '(1 220.0 20.0))

    (let ((w (~> *model* button-plus)))
      (assert (equal "+" (label w)))
      ;; (warn "widget ~S" w)
      (assert (equal (cons 10 10) (top-left-abs w))))

    (let ((w (~> *model* button-minus)))
      (assert (equal "-" (label w)))
      (assert (equal (cons 210 10) (top-left-abs w))))

    (process-gtk-event win :RESIZE '(150 390))

    (let ((w (~> *model* button-plus)))
      (assert (equal "+" (label w)))
      ;; (warn "widget ~S" w)
      (assert (equal (cons 10 10) (top-left-abs w))))

    (let ((w (~> *model* button-minus)))
      (assert (equal "-" (label w)))
      (assert (equal (cons 10 210) (top-left-abs w))))

    (process-gtk-event win :RESIZE '(600 400))

    (let ((w (~> *model* button-plus)))
      (assert (equal "+" (label w)))
      ;; (warn "widget ~S" w)
      (assert (equal (cons 10 10) (top-left-abs w))))

    (let ((w (~> *model* button-minus)))
      (assert (equal "-" (label w)))
      (assert (equal (cons 210 10) (top-left-abs w))))

    (warn "finished test-experiment")))

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
