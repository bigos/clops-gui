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

;; *** component
;; **** external vs internal
;; has external part exposed to the client, that should be stable
;; and internal (private)
;; The key is to find which parts should not be exposed to the external client
;; **** responsibilities
;; component may have specific responsibilities
;; interface defined the behaviour of the component from the standpoint of the client

;;; utilities !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

;; TODO add print-object and better inspector

;;; classes !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
;;; existing classes gui-app and gui-window

;; TODO add classes for new implementation of boxes

;;; interfaces !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

;;; components !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

;;; drawing is a component used by GTK to draw on canvas
;;; events is a component for responding to GTK events

;;; description of another component and its implementation will go here

;;; drawing ====================================================================
(defclass/std resizing-sections-window (gui-window:lisp-window) (()))

;; In main function we tell to use draw-window to draw on canvas
(defmethod draw-window ((window resizing-sections-window))
  ;; paint background
  (let ((cv 0.95)) (cairo:set-source-rgb  cv cv cv))
  (cairo:paint)

  (cairo:select-font-face "Ubuntu Mono" :italic :bold)
  (cairo:set-font-size 10)
  (cairo:move-to 10 10)
  (gui-window:set-rgba "black")
  (cairo:show-text (format nil "Try to resize the window and see how the elements respond to resizing"))


  (cairo:select-font-face "Ubuntu Mono" :italic :bold)
  (cairo:set-font-size 15)
  (cairo:move-to 10 100)
  (let ((cmotion    (gui-app:current-motion-window-p gui-app:*lisp-app* window)))
    (if cmotion
        (gui-window:set-rgba "green")
        (gui-window:set-rgba "red"))
    (cairo:show-text (format nil "dimensions ~A" (gui-window:dimensions window))))

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
       (declare (ignore button))
       ))
    (:released
     (gui-app:mouse-button-released))
    (:scroll)
    (:resize
     (destructuring-bind ((w h)) args
       (gui-window:window-resize w h lisp-window)))
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

(main)
