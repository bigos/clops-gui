;;;
(in-package #:gui-events)

(defparameter *process-event-fn* nil)

;;; ================================= All defined GUI Events ===========================

(defun de-menu-simple  (action-name)           (process-event :menu-simple action-name))
(defun de-menu-bool    (action-name bool01)    (process-event :menu-bool   action-name bool01))
(defun de-menu-radio   (action-name radiostr)  (process-event :menu-radio  action-name radiostr))
(defun de-motion       (x y window)            (process-event :motion       x y window))
(defun de-motion-enter (x y window)            (process-event :motion-enter x y window))
(defun de-focus-enter  (window)                (process-event :focus-enter  window))
(defun de-motion-leave (window)                (process-event :motion-leave window))
(defun de-focus-leave  (window)                (process-event :focus-leave  window))
(defun de-pressed      (button x y)            (process-event :pressed   button x y))
(defun de-released     (button x y)            (process-event :released  button x y))
(defun de-scroll       (dx dy)                 (process-event :scroll    dx dy))
(defun de-key-pressed  (letter name code mods) (process-event :key-pressed   letter name code mods))
(defun de-key-released (letter name code mods) (process-event :key-released  letter name code mods))
(defun de-resize       (width height window)   (process-event :resize width height window))
(defun de-timeout      ()                      (process-event :timeout))

;;; ======================= update and view ====================================

(defun process-event (event &rest args)
  (if (null *process-event-fn*)
      (error "implement process-event")
      (funcall *process-event-fn* event args)))
