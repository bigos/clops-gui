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
   prepare-submenu))

(defpackage #:gui-window
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
   *lisp-app*
   *timeout-period*
   add-child
   all-widgets
   all-windows
   children
   close-all-windows-and-quit
   current-focus
   current-focus-window
   current-motion
   current-motion-window
   dimensions
   gir-window
   lisp-window
   most-current-widget
   mouse-button
   mouse-button-pressed
   mouse-button-released
   mouse-coordinates
   mouse-motion-enter
   mouse-motion-leave
   new-window-for-app
   present-about-dialog
   redraw-canvas
   set-rgba
   simulate-draw-func
   window
   window-creation-from-menu
   window-creation-from-simulation
   window-get
   window-hkey
   window-resize
   windows
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
   de-timeout))

(defpackage #:gui-box
  (:use #:cl)
  (:import-from :serapeum
                ~>)
  (:import-from :defclass-std
                defclass/std)
  (:export
   absolute-coordinates
   absolute-x
   absolute-y
   add-child
   remove-child
   box
   box-color
   children
   coordinates
   height
   mouse-over-score
   mouse-overp
   mouse-score
   move-to
   parent
   parent-boxes
   recalculate-absolute
   resize
   root-window
   text
   text-box
   top-left
   width
   x
   y
   ;; add more
   ))
