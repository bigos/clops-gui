(defpackage #:gui-menu
  (:use #:cl)
  (:export
   build-items
   build-menu
   prepare-item-bool
   prepare-item-radio
   prepare-item-simple
   prepare-radio-action
   prepare-section
   prepare-submenu
   ))

(defpackage #:gui-app
  (:use #:cl)
  (:import-from :serapeum
                ~>)
  (:import-from :defclass-std
                defclass/std)
  (:export
   *lisp-app*
   all-windows
   all-windows
   current-focus
   current-focus-window
   current-motion
   current-motion-window-p
   gtk4-app
   make-lisp-app
   mouse-button
   mouse-button-pressed
   mouse-button-released
   mouse-coordinates
   mouse-motion-enter
   mouse-motion-leave
   window-get
   window-remove
   windows
   ))

(defpackage #:gui-window
  (:use #:cl)
  (:import-from :serapeum
                ~>)
  (:import-from :defclass-std
                defclass/std)
  (:export
   *client-fn-draw-objects*
   *client-fn-menu-bar*
   *timeout-period*
   add-child
   all-widgets
   children
   dimensions
   gir-window
   lisp-window
   most-current-widget
   new-window-for-app
   redraw-canvas
   set-rgba
   window-hkey
   window-resize
   ))
(defpackage #:gui-window-gtk
  (:use #:cl)
  (:import-from :serapeum
                ~>)
  (:import-from :defclass-std
                defclass/std)
  (:export
   *client-fn-draw-objects*
   *client-fn-menu-bar*
   *initial-title*
   *initial-window-height*
   *initial-window-width*
   *timeout-period*
   close-all-windows-and-quit
   gir-window
   new-window-for-app
   present-about-dialog
   window
   window-creation-from-menu
   window-creation-from-simulation
   ))

(defpackage #:gui-events
  (:use #:cl)
  (:import-from :serapeum
                ~>)
  (:import-from :defclass-std
                defclass/std)
  (:export
   *client-fn-process-event*
   de-focus-enter
   de-focus-leave
   de-key-pressed
   de-key-released
   de-menu-bool
   de-menu-radio
   de-menu-simple
   de-motion
   de-motion-enter
   de-motion-leave
   de-pressed
   de-released
   de-resize
   de-scroll
   de-timeout
   ))

(defpackage #:gui-drawing
  (:use #:cl)
  (:export
   %draw-func
   *client-fn-draw-objects*
   simulate-draw-func
   ))

(defpackage #:gui-box
  (:use #:cl)
  (:import-from :serapeum
                ~>)
  (:import-from :defclass-std
                defclass/std)
  (:export
   absolute-x
   absolute-y
   add-child
   box
   box-color
   box:text
   central-point
   children
   mouse-over-score
   mouse-overp
   move-to
   parent
   recalculate-absolute
   remove-child
   resize
   text
   text-box
   x
   y
   ;; add more
   ))

(defpackage #:gui-color
  (:use #:cl)
  (:export
   set-rgba
   ))
