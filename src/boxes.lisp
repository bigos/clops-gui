(declaim (optimize (speed 0) (safety 3) (debug 3)))

#| play with Morphic inspired UI
looking for simple ways to have something that responds to resizing, miving
and layout changes, possibly for listing files and preparing infrastructure for
adding more features
|#

;; (ql:quickload :window-canvas-boxes)
;; (boxes:main)

(in-package #:common-lisp)

(in-package #:boxes)

;;; ============================================================================
;;; --- macros -----------------------------------------------------------------

;;; --- defparameter et al -----------------------------------------------------

(defparameter *model* nil)

;;; --- defgenerics (grouped by protocol) --------------------------------------

;;; --- classes ----------------------------------------------------------------
(with-base-defclass coordinates () ((x :type integer) (y :type integer))
  coordinates-absolute
  coordinates-relative)


(defclass/std boxes-window (window-canvas:lisp-window)
  ((width)
   (height)))
;;; and more classes in a separate file

(defclass/std box       ()
  ((parent)
   (coordinates-relative :type coordinates-relative)
   (coordinates-absolute :type coordinates-absolute)
   (width)
   (height)
   (color)
   (children)))

(defclass/std node (box)
  ())

(defclass/std node-text (box)
  ((text)
   (wrap :std 'truncate :type symbol)))

(defclass/std node-character (box)
  ((bchar)
   (font-size :std 15)
   (font-color :std "black")
   (font-face :std "Ubuntu Mono")
   (font-slant :std :normal)
   (font-weight :std :normal)))

(defclass/std node-horizontal (node)
  ())

(defclass/std node-vertical (node)
  ())

(defclass/std node-right (node-horizontal)
  ())

(defclass/std node-left (node-horizontal)
  ())

(defclass/std node-down (node-vertical)
  ())

(defclass/std node-up (node-vertical)
  ())

;;; --- inspectors -------------------------------------------------------------
(defun print-object-inner (obj  stream)
  (print-unreadable-object (obj stream :type t :identity t)
    (format stream "~a"
            (loop for sl in (sb-mop:class-slots (class-of obj))
                  for slot-name = (sb-mop:slot-definition-name sl)
                  collect (cons slot-name
                                (if (slot-boundp obj slot-name)
                                    (format nil "~S" (slot-value obj slot-name))
                                    (format nil "---unbound---" )))))))

;;; replace T with concrete classes
(defmethod print-object ((obj T) stream)
  (print-object-inner obj stream))

(defmethod print-object ((obj box) stream)
  (print-unreadable-object (obj stream :type t :identity t)
    (format stream "~a - ~S ~S"
            (color obj)
            (if (slot-boundp obj 'coordinates-absolute)
                (cons (x (coordinates-absolute obj))
                      (y (coordinates-absolute obj)))
                :unbound-absolute)
            (if (slot-boundp obj 'coordinates-relative)
                (cons (x (coordinates-relative obj))
                      (y (coordinates-relative obj)))
                :unbount-relative)
            )))

(defmethod print-object ((obj coordinates) stream)
  (print-object-inner obj stream))

;;; --- constructors -----------------------------------------------------------

;;; --- methods ----------------------------------------------------------------

;;; === draw window ============================================================
(defun window-snap-shot (changeme)
  (warn "== screen shot")
  ;; TODO change it
  (setf gui-window:*window-size-cons-code* (lambda () (cons 430 490)))
  (window-canvas::simulate-draw-func))

(defun show-my-text (x y font-size str &optional (color "black"))
  (window-canvas:set-rgba color)
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
  (assert (or (typep window-id 'integer)
              (typep window-id 'keyword)))
  (let ((window (window-canvas:get-window window-id)))
    (assert (typep window 'window-canvas:lisp-window))

    ;; paint background
    (let ((cv 0.9))
      (cairo:set-source-rgb  cv cv cv)
      (cairo:paint))

    ;; more to do
    (show-my-text 10 30 15 (format nil "Please code something"))

    ;; and more - dom structure

    ;; and more - drawing it
    (let ((world (make-node 0 0 (width window) (height window) "#cccccc88")))
      (absolute-coordinates world)

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
      (absolute-coordinates world)

      ;; (warn "rendering -----------------------------------------------")
      (render world))))

;;; === events =================================================================
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
            (setf window-canvas:*window-size-cons-code* (lambda () (cons
                                                                    width
                                                                    height)))
            (setf (width  lisp-window) width
                  (height lisp-window) height)))
    (:key-pressed
     (destructuring-bind ((entered key-name key-code mods)) args
       (warn "key pressed ~S~%" (list entered key-name key-code mods))
       (when (equal key-name "Pause")
         (window-snap-shot nil))
       ;; (game-key-pressed *game* entered key-name)
       (move-b1 *model* key-name)
       ))
    (otherwise
     (warn "not handled event ~S ~S" event args)))

  (window-canvas:redraw-canvas lisp-window (format  nil "~A" event)))

;;; === main ===================================================================
(defun init ()
  (progn
    (setf window-canvas:*gtk-client-fn-draw-objects*  'boxes::draw-window)
    (setf window-canvas:*gtk-client-fn-menu-bar*      nil)
    (setf window-canvas:*gtk-client-fn-process-event* 'boxes::process-gtk-event)
    (setf window-canvas:*gtk-initial-window-width*    790)
    (setf window-canvas:*gtk-initial-window-height*   750)
    (setf window-canvas:*gtk-initial-title*           "Boxes")
    (setf *model* (make-model))

    ))

(defun main ()
  (format T "boxes main~%")
  (init)
  (window-canvas:remove-all-windows)
  (window-canvas:window-main (make-instance 'boxes-window
                                            :width  window-canvas:*gtk-initial-window-width*
                                            :height window-canvas:*gtk-initial-window-height*)))

;;; testing ====================================================================
(defun experiment-init ()
  (format T "boxes main~%")
  (init)
  (window-canvas::window-creation-from-simulation :testing
                                                  (make-instance 'boxes-window
                                                                 :width  window-canvas:*gtk-initial-window-width*
                                                                 :height window-canvas:*gtk-initial-window-height*)))

;; (experimental-run)
(defun experimental-run ()
  (experiment-init)
  (let ((window (window-canvas:get-window :testing)))
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
