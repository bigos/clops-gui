(defpackage #:gui-menu
  (:use #:cl)
  (:export
   :build-items
   :build-menu
   :prepare-item-bool
   :prepare-item-radio
   :prepare-item-simple
   :prepare-radio-action
   :prepare-section
   :prepare-submenu))

(defpackage #:gui-window
  (:use #:cl)
  (:import-from :defclass-std
   :defclass/std)
  (:export
   :*client-fn-draw-objects*
   :*initial-title*
   :*initial-window-width*
   :*initial-window-height*
   :*lisp-app*
   :*client-fn-menu-bar*
   :all-windows
   :close-all-windows-and-quit
   :current-motion
   :current-focus
   :current-motion-window
   :current-focus-window
   :dimensions
   :gir-window
   :lisp-window
   :mouse-coordinates
   :new-window-for-app
   :present-about-dialog
   :redraw-canvas
   :set-rgba
   :simulate-draw-func
   :window
   :window-creation-from-menu
   :window-get
   :window-hkey
   :window-resize
   :windows
   ))

(defpackage #:gui-events
  (:use #:cl)
  (:import-from :serapeum
   :~>)
  (:import-from :defclass-std
   :defclass/std)
  (:export
   :*client-fn-process-event*
   :de-focus-enter
   :de-focus-leave
   :de-key-pressed
   :de-key-released
   :de-menu-bool
   :de-menu-radio
   :de-menu-simple
   :de-motion
   :de-motion-enter
   :de-motion-leave
   :de-pressed
   :de-released
   :de-resize
   :de-scroll
   :de-timeout))

(defpackage #:gui-box
  (:use #:cl)
  (:import-from :serapeum
   :~>)
  (:import-from :defclass-std
   :defclass/std)
  (:export
   ;; classes
   :box
   :text-box
   :coordinates
   :x
   :y
   :text
   :top-left
   ;; add more
   ))
