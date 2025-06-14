(defsystem "clops-gui"
  :version "0.0.1"
  :author "https://github.com/bigos"
  :license "PUBLIC DOMAIN"
  :depends-on (#:cl-gtk4
               #:cl-gdk4 #:cl-glib #:cl-cairo2
               #:serapeum
               #:defclass-std
               )
  :components ((:module "src"
                :components
                        ((:file "packages")
                         (:file "boxes")
                         (:file "boxes-ui")
                         (:file "gui-color")
                         (:file "gui-events")
                         (:file "gui-drawing")
                         (:file "gui-menu")
                         (:file "gui-box")
                         (:file "gui-window")
                         (:file "gui-app")
                         (:file "gui-window-gtk")
                         (:file "window-canvas")
                         )))
  :description "GUI for clops in separate system")

;;; ----------------- test --------------------------
(asdf:defsystem "clops-gui/tests"
  :depends-on (#:clops-gui #:fiveam)
  :pathname "tests/"
  :components ((:file "package")
               (:file "clops-gui-tests"))
  :perform (test-op (op c)
                    (uiop:symbol-call :fiveam :run-all-tests)))

;; (ql:quickload :clops-gui/tests)
;; (asdf:test-system :clops-gui/tests)
