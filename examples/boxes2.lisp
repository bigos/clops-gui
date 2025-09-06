(declaim (optimize (speed 0) (safety 3) (debug 3)))

#|
I need to replace the relavant window canvas calls to gui-* calls
|#

;; (load "~/Programming/Lisp/clops-gui/examples/boxes2.lisp")

(ql:quickload :clops-gui)

(in-package #:common-lisp)

(defpackage #:boxes2
  (:use #:cl)
  ;; imports
  (:import-from
   :boxes
   ;; ---
   :*model*
   :make-coordinates-relative
   :make-coordinates-absolute
   :make-model
   :make-node
   :make-node-left
   :make-node-right
   :make-node-up
   :make-node-down
   :make-node-auto
   :add-children
   :node-text
   :node-character
   :bbx
   :bby
   :grw
   :grh
   :tbw
   :mouse-position
   :text
   :wrap
   :width
   :height
   :render
   :boxes-window)
  ;; exports
  (:export #:main))

(in-package #:boxes2)


;;; === draw window ============================================================
(defun window-snap-shot (lisp-window)
  (warn "== screen shot ~S" lisp-window)
  ;; TODO think of removing
  ;;(setf window-canvas:*window-size-cons-code* (lambda () (cons 430 490)))
  (gui-drawing::simulate-draw-func lisp-window))

(defun show-my-text (x y font-size str &optional (color "black"))
  (gui-color:set-rgba color)
  (cairo:select-font-face "Ubuntu Mono" :normal :normal)
  (cairo:set-font-size font-size)
  (cairo:move-to x y)
  (cairo:show-text str))

(defun text-size (text text-size)
  (cairo:select-font-face "Ubuntu Mono" :normal :normal)
  (cairo:set-font-size text-size)

  (multiple-value-bind (xb yb width height)
      (handler-bind
          ((alexandria:simple-style-warning
             (lambda (warning)
               (when (alexandria:starts-with-subseq
                      "bare references to struct types are deprecated."
                      (simple-condition-format-control warning))
                 (muffle-warning warning)))))

        (cairo:text-extents (format nil "~A" text)))
    (list :xb xb :yb yb :width width :height height)))

(defun add-children-text (world text-string)
  (let ((text-container (make-node 20
                                   340
                                   (- (width world) 20 20)
                                   (- (height world) 360) "yellow")))
    (add-children text-container
                  (let* ((fsize 18)
                         (margin-horizontal 0)
                         (margin-vertical 0)

                         (text-data (text-size "ly()" fsize ))
                         (twidth (floor (/ (getf text-data :width) 4)))
                         (theight (getf text-data :height))

                         (bwidth (+ twidth 0))
                         (bheight (+ theight 0)))
                    (loop for last-char = nil then c
                          for row = 0 then (if (equal last-char #\Newline) (1+ row) row)
                          for col = 0 then (if (equal last-char #\Newline)  0 (1+ col))
                          for relx = (+ margin-horizontal
                                        (ceiling
                                         (* col  (1+ twidth) )))
                          for rely = (+ margin-vertical
                                        (ceiling
                                         (* row (1+ theight))))
                          for c across text-string
                          unless (let ((max-x-coord (+ relx bwidth))
                                       (max-y-coord (+ rely bheight)))
                                   (or
                                    (>= max-x-coord (- (width text-container) 10))
                                    (>= max-y-coord (height text-container))))
                            collect (make-instance 'node-character
                                                   :bchar c
                                                   :font-size fsize
                                                   :coordinates-relative
                                                   (make-coordinates-relative
                                                    relx
                                                    rely)
                                                   :width bwidth
                                                   :height bheight
                                                   :color "pink"))))))

(defun draw-window (window-id)
  (let ((window (gui-app:window-get gui-app:*lisp-app* window-id)))
    (assert (typep window 'gui-window:lisp-window))

    ;; paint background
    (let ((cv 0.9))
      (cairo:set-source-rgb  cv cv cv)
      (cairo:paint))

    ;; more to do
    (show-my-text 10 30 15 (format nil "Please code something"))

    ;; and more - dom structure

    ;; and more - drawing it
    (let ((world (make-node 0 0 (width window) (height window) "#cccccc88")))
      ;; TODO possibly remove it
      (boxes:absolute-coordinates world)

      (add-children world
                    (list
                     (add-children (make-node (bbx *model*)
                                              (bby *model*)
                                              50 50 "purple")
                                   (list
                                    (make-node 10 10 25 25 "violet")))))
      ;; --------------
      ;; https://www.w3.org/TR/SVG11/types.html#ColorKeywords
      ;;(warn "------------------------ adding tree nodes ------------------------------")
      (add-children world
                    (list
                     (add-children (make-node-right 20 80 100 100 "black")
                                   (list
                                    (make-node-auto 20 30 "red")
                                    (make-node-auto (grw *model*) (grh *model*) "green")
                                    (make-node-auto 40 30 "blue")))
                     (add-children (make-node-left 220 80 100 100 "black")
                                   (list
                                    (make-node-auto 20 30 "red")
                                    (make-node-auto (grw *model*) (grh *model*) "green")
                                    (make-node-auto 40 30 "blue")))

                     (add-children (make-node-down 20 220 101 101 "grey")
                                   (list
                                    (make-node-auto 30 20 "red")
                                    (make-node-auto (grw *model*) (grh *model*) "green")
                                    (make-node-auto 30 40 "blue")))
                     (add-children (make-node-up 220 220 101 101 "grey")
                                   (list
                                    (make-node-auto 30 20 "red")
                                    (make-node-auto (grw *model*) (grh *model*) "green")
                                    (make-node-auto 30 40 "blue")))

                     (add-children (make-node (floor (/ (width world) 2))
                                              50
                                              (- (floor (/ (width world) 2)) 20)
                                              (+ 0 280) "mediumseagreen")
                                   (list
                                    (make-instance 'node-text
                                                   :coordinates-relative (make-coordinates-relative 10 10)
                                                   :width (tbw *model*)
                                                   :height 30
                                                   :color "yellow"
                                                   :wrap 'truncate
                                                   :text "Trimming the first text was easy."
                                                   )
                                    (make-instance 'node-text
                                                   :coordinates-relative (make-coordinates-relative 10 50)
                                                   :width (tbw *model*)
                                                   :height 30
                                                   :color "white"
                                                   :wrap 'wrap
                                                   :text "Wrapping the second text is more challenging."
                                                   )
                                    ))
                     (add-children-text world
                                        (alexandria:read-file-into-string
                                         (let ((text-option 1))
                                           (case text-option
                                             (0 "~/Documents/Roams/README.org")
                                             (1 "~/Programming/Lisp/lispy-experiments/window-canvas-boxes/window-canvas-boxes.asd")
                                             (T "~/.bashrc"))))
                                        )))
      ;; === starting from world set absolute coordinates ===
      ;; (warn "adding absolute coordinates -----------------------------------")
      (boxes:absolute-coordinates world)

      ;; (warn "rendering -----------------------------------------------")
      (render world))))

;;; === events =================================================================

(defmethod move-b1 ((model model) direction)
  (cond ((equal direction "Left")
         (setf (bbx model) (- (bbx model) 5)))
        ((equal direction "Right")
         (setf (bbx model) (+ (bbx model) 5)))

        ((equal direction "q")
         (setf (grw model) (- (grw model) 5)))
        ((equal direction "w")
         (setf (grw model) (+ (grw model) 5)))

        ((equal direction "b")
         (setf (grh model) (- (grh model) 5)))
        ((equal direction "h")
         (setf (grh model) (+ (grh model) 5)))

        ((equal direction "a")
         (setf (tbw model) (- (tbw model) 5)))
        ((equal direction "s")
         (setf (tbw model) (+ (tbw model) 5)))

        (T
         (warn "un-handled key"))))


(defmethod process-gtk-event ((lisp-window boxes-window) event &rest args)
  (unless (member event '(:timeout :motion))
    (warn "event ~S ~S" event args))

  (case event
    (:timeout
          ;; do nothing yet
     )
    ((:motion :motion-enter)
     ;; we use simple case with one window so we ignore the window argument
     (destructuring-bind ((x y)) args
       (setf (mouse-position *model*) (cons x y))))
    (:motion-leave
     (setf (mouse-position *model*) nil))
    (:focus-enter)
    (:focus-leave)
    (:pressed
          (destructuring-bind ((button x y)) args
            (warn "pressed button ~A at ~a ~a" button x y)
            ;; (mouse-press *game* button x y)
            )
     )
    (:released
          (destructuring-bind ((button x y)) args
            (warn "released button ~A at ~a ~a" button x y)
            ;; (mouse-release *game* button x y)
            ))
    (:scroll)
    (:resize
          (destructuring-bind ((width height)) args
            (warn "resized with ~s ~s" width height)
            ;; TODO fix later
            ;; (setf window-canvas:*window-size-cons-code* (lambda () (cons
            ;;                                                         width
            ;;                                                         height)))
            (setf (width  lisp-window) width
                  (height lisp-window) height
                  (gui-window:dimensions lisp-window) (cons width height))))
    (:key-pressed
     (destructuring-bind ((entered key-name key-code mods)) args
       (warn "key pressed ~S~%" (list entered key-name key-code mods))
       (when (equal key-name "Pause")
         (window-snap-shot lisp-window))
       ;; (game-key-pressed *game* entered key-name)
       (move-b1 *model* key-name)
       ))
    (:menu-simple
     (destructuring-bind ((menu-item)) args
       (warn "pressed menu item ~a" menu-item)
       (cond
         ((equal menu-item "quit")
          (gui-window-gtk:close-all-windows-and-quit))
         (T
          (warn "not handled menu-item ~S" menu-item)))
       ))
    (otherwise
     (warn "not handled event ~S ~S" event args)))

  (gui-window:redraw-canvas lisp-window (format  nil "~A" event)))

;;; === main ===================================================================

(defun menu-bar (app lisp-window)
    (let ((menu (gio:make-menu)))
      (gui-menu:build-menu
       menu
       (gui-menu:prepare-submenu
        "File"
        (gui-menu:prepare-section
         nil
         (gui-menu:build-items
          (gui-menu:prepare-item-simple lisp-window app menu "Quit" "quit"))))
       (gui-menu:prepare-submenu
        "Help"
        ;; for now I plan to have only the About menu item
        (gui-menu:prepare-section
         nil
         (gui-menu:build-items
          (gui-menu:prepare-item-simple lisp-window app menu "About" "about")))))

      (values menu)))

(defun init ()
  (progn
    (setf gui-drawing:*client-fn-draw-objects*  'boxes2::draw-window)
    (setf gui-window-gtk:*client-fn-menu-bar*      nil)
    (setf gui-events:*client-fn-process-event* 'boxes2::process-gtk-event)
    (setf gui-window-gtk:*initial-window-width*    790)
    (setf gui-window-gtk:*initial-window-height*   750)
    (setf gui-window-gtk:*initial-title*           "Boxes")
    (setf gui-window-gtk:*client-fn-menu-bar* 'boxes2::menu-bar)
    (setf *model* (make-model))

    ))

(defun main ()
  (format T "boxes main~%")
  (init)

  (gui-window-gtk:window-main
   (make-instance 'boxes-window
                  :width  gui-window-gtk:*initial-window-width*
                  :height gui-window-gtk:*initial-window-height*)))

;;; testing ====================================================================
(defun experiment-init ()
  (format T "boxes main~%")
  (init)
  (gui-window-gtk::window-creation-from-simulation :testing
                                                   (make-instance 'boxes-window
                                                                  :width  gui-window-gtk:*initial-window-width*
                                                                  :height gui-window-gtk:*initial-window-height*)))

;; (experimental-run)
(defun experimental-run ()
  (experiment-init)
  (let ((window (gui-app:window-get gui-app:*lisp-app* :testing)))
    (process-gtk-event window :resize '(400 500))
    (process-gtk-event window :timeout)
    (process-gtk-event window :resize '(500 600))
    (process-gtk-event window :key-pressed '("" "Right" 114 NIL))
    (process-gtk-event window :key-pressed '("w" "w" 114 NIL))
    (process-gtk-event window :key-pressed '("w" "w" 114 NIL))
    (process-gtk-event window :key-pressed '("w" "w" 114 NIL))
    (process-gtk-event window :key-pressed '("w" "w" 114 NIL))
    (process-gtk-event window :key-pressed '("w" "w" 114 NIL))

    (process-gtk-event window :key-pressed '("h" "h" 114 NIL))
    (process-gtk-event window :key-pressed '("h" "h" 114 NIL))
    (process-gtk-event window :key-pressed '("h" "h" 114 NIL))
    (process-gtk-event window :key-pressed '("h" "h" 114 NIL))
    (process-gtk-event window :key-pressed '("h" "h" 114 NIL))
    (process-gtk-event window :key-pressed '("h" "h" 114 NIL))
    (process-gtk-event window :key-pressed '("h" "h" 114 NIL))
    ))

(main)
