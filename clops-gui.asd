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
                 (:file "gui-menu")
                 (:file "gui-window"))))
  :description "GUI for clops in separate system")

;; (push #p "~/Programming/Lisp/clops-gui/" ql:*local-project-directories*)
;; (ql:quickload :clops-gui)
