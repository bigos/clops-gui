(defsystem "clops-gui"
  :version "0.0.1"
  :author "https://github.com/bigos"
  :license "PUBLIC DOMAIN"
  :depends-on (#:cl-gtk4
               #:cl-gdk4 #:cl-glib #:cl-cairo2
               #:serapeum
               #:defclass-std)
  :components ((:module "src"
                :components
                ((:file "packages")
                 (:file "gui-events")
                 (:file "gui-drawing")
                 (:file "gui-menu")
                 (:file "gui-box")
                 (:file "gui-window")
                 )))
  :description "GUI for clops in separate system")
