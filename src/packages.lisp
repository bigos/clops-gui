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
   :*draw-objects-fn*
   :*menu-bar-menu-fn*
   :*lisp-app*
   :windows
   :gir-window
   :simulate-draw-func
   :lisp-window
   :present-about-dialog :new-window-for-app :close-all-windows-and-quit
   :window))

(defpackage #:gui-events
  (:use #:cl)
  (:import-from :serapeum :~>)
  (:import-from :defclass-std :defclass/std)
  (:export
   :*process-event-fn*
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
