(declaim (optimize (speed 0) (safety 3) (debug 3)))
;;;
(in-package #:gui-events)

(defparameter *client-fn-process-event* nil)

;;; ================================= All defined GUI Events ===========================

(defun de-menu-simple  (lisp-window action-name)           (process-event lisp-window :menu-simple action-name))
(defun de-menu-bool    (lisp-window action-name bool01)    (process-event lisp-window :menu-bool   action-name bool01))
(defun de-menu-radio   (lisp-window action-name radiostr)  (process-event lisp-window :menu-radio  action-name radiostr))
(defun de-motion       (lisp-window x y)                   (process-event lisp-window :motion       x y))
(defun de-motion-enter (lisp-window x y)                   (process-event lisp-window :motion-enter x y))
(defun de-focus-enter  (lisp-window)                       (process-event lisp-window :focus-enter  ))
(defun de-motion-leave (lisp-window)                       (process-event lisp-window :motion-leave))
(defun de-focus-leave  (lisp-window)                       (process-event lisp-window :focus-leave))
(defun de-pressed      (lisp-window button x y)            (process-event lisp-window :pressed   button x y))
(defun de-released     (lisp-window button x y)            (process-event lisp-window :released  button x y))
(defun de-scroll       (lisp-window dx dy)                 (process-event lisp-window :scroll    dx dy))
(defun de-key-pressed  (lisp-window letter name code mods) (process-event lisp-window :key-pressed   letter name code mods))
(defun de-key-released (lisp-window letter name code mods) (process-event lisp-window :key-released  letter name code mods))
(defun de-resize       (lisp-window width height)          (process-event lisp-window :resize width height))
(defun de-timeout      (lisp-window )                      (process-event lisp-window :timeout))

;;; ======================= update and view ====================================

(defun process-event (lisp-window event &rest args)
  (if (null *client-fn-process-event*)
      (error "implement process-event")
      (funcall *client-fn-process-event* lisp-window event args)))
