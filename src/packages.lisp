(defpackage #:gui-menu
  (:use #:cl)
  (:export
   :build-menu :build-items
   :prepare-submenu :prepare-section :prepare-item-simple :prepare-item-bool
   :prepare-item-radio :prepare-radio-action))

(defpackage #:gui-window
  (:use #:cl)
  (:import-from :defclass-std :defclass/std)
  (:export
   :present-about-dialog :new-window-for-app :close-all-windows-and-quit
   :window))

(defpackage #:clops
  (:use #:cl)
  (:import-from :serapeum :~>)
  (:import-from :defclass-std :defclass/std)
  (:export
   :draw-objects
   :nebu-bar-menu
   :*lisp-app*
   :box-div
   :build-box-class
   :gtk4-app
   :lisp-app
   :menu-bar-menu
   :window-add
   :window-remove))

(defpackage #:gui-events
  (:use #:cl)
  (:import-from :serapeum :~>)
  (:import-from :defclass-std :defclass/std)
  (:export
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

(defpackage clops-gui
  (:use :cl))
