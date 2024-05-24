(declaim (optimize (speed 0) (safety 2) (debug 3)))

;; (load "~/Programming/Lisp/clops-gui/examples/pong.lisp")

;;; === load ===================================================================
(push #p "~/Programming/Lisp/clops-gui/" ql:*local-project-directories*)
(ql:quickload '(:clops-gui) :silent nil)

;;; === package ================================================================
(defpackage #:pong
  (:use #:cl)
  (:import-from :serapeum
   :~>)
  (:import-from :defclass-std
   :defclass/std))

(in-package #:pong)

(defparameter *pong-game* nil)

;;; ======================= experiment =========================================
(defun experiment-first-window ()
  (setf
   gui-window:*client-fn-draw-objects*  'pong::draw-window
   gui-window:*lisp-app* (make-instance 'gui-window::lisp-app))

  (assert (zerop (hash-table-count (gui-window:all-windows))))
  (let ((lisp-window (make-instance 'pong-window)))
    (gui-window::window-creation-from-simulation :testing lisp-window)
    (assert (eq 1 (hash-table-count (gui-window:all-windows))))
    lisp-window))

(defun experiment ()
  (warn "starting experiment")

  (let ((lisp-window (experiment-first-window)))
    (process-event lisp-window :resize (list 600 200 ))
    (process-event lisp-window :timeout)
    (process-event lisp-window :motion-enter (list 50 50 ))

    (process-event lisp-window :key-pressed (list "" "Escape" 9  nil))
    (process-event lisp-window :key-pressed (list "" "F1" 67     nil))
    (process-event lisp-window :key-pressed (list "r" "r" 10     nil))
    (process-event lisp-window :key-pressed (list " " "space" 65 nil))))

;;; === defgenerics ============================================================

;;; === classes ================================================================
(defclass/std pong-window (gui-window:lisp-window)
  (()))

(defclass/std tutorial-window (gui-window:lisp-window)
  (()))

(defclass/std player ()
  ((pad-height)
   (pad-y)))

(defclass/std player-human (player)
  ((name)))

(defclass/std player-computer (player)
  ((model)))

(defclass/std slider ()
  (()))

(defclass/std slider-human (slider)
  (()))

(defclass/std slider-computer (slider)
  (()))

(defclass/std ball ()
  ((coordinates)
   (radius)))

(defclass/std game-area ()
  ((top-left)
   (bottom-right)))

(defclass/std pong-game ()
  ((player-human    :a :std (make-instance 'player-human))
   (player-computer :a :std (make-instance 'player-computer))
   (ball            :a :std (make-instance 'ball))
   (game-area       :a :std (make-instance 'game-area))
   (pong-window)))

;;; === methods ================================================================
(defmethod initialize-instance :after ((pong-game pong-game) &rest initargs &key)
  (declare (ignore initargs))
  (warn "running after initialize instance")

  (let* ((top-line  30)
         (left-line 30)
         (right-line  (- gui-window:*initial-window-width*  30))
         (bottom-line (- gui-window:*initial-window-height* (* 2 30))))
    (setf
     (player-human pong-game)    (make-instance 'player-human
                                                :pad-height 100
                                                :pad-y 150)
     (player-computer pong-game) (make-instance 'player-computer
                                                :pad-height 100
                                                :pad-y 120)
     (ball pong-game)            (make-instance 'ball
                                                :coordinates (make-instance 'gui-box:coordinates
                                                                            :x 50
                                                                            :y 50)
                                                :radius 5)
     (game-area pong-game)       (make-instance 'game-area
                                                :top-left (make-instance 'gui-box:coordinates
                                                                         :x left-line
                                                                         :y top-line)
                                                :bottom-right (make-instance 'gui-box:coordinates
                                                                             :x right-line
                                                                             :y bottom-line)))))

(defmethod start-game ((game pong-game))
  (warn "starting pong"))

(defmethod resize ((game pong-game) w h)
  (warn "resizing pong ~S" game))

(defmethod width ((game-area game-area))
  (- (~> game-area bottom-right gui-box:x)
     (~> game-area top-left     gui-box:x)))

(defmethod height ((game-area game-area))
  (- (~> game-area bottom-right gui-box:y)
     (~> game-area top-left     gui-box:y)))

(defmethod mouse-set-human-pad-y ((pong-game pong-game) py)
  (setf (~> pong-game player-human pad-y) (- py (~> pong-game game-area top-left gui-box:y ))))

;;; --- rendering -------------------------------
(defmethod render ((ball ball))
  (cairo:set-line-width 0.9)
  (cairo:arc (~> ball coordinates gui-box:x)
             (~> ball coordinates gui-box:y)
             (~> ball radius)
             0
             (* 2 PI))
  (gui-window:set-rgba "blue")          ;fill
  (cairo:fill-preserve)
  (gui-window:set-rgba "red")           ;outline
  (cairo:stroke))

(defmethod render ((game-area game-area))
  (gui-window:set-rgba "#FFFF0088")
  (cairo:rectangle
   (~> game-area top-left gui-box:x)
   (~> game-area top-left gui-box:y)
   (~> game-area width)
   (~> game-area height))
  (cairo:fill-path))

(defmethod render ((player-human player-human))
  (gui-window:set-rgba "blue")

  (let ((block-width 25))
    (cairo:rectangle
     (- (~> *pong-game* game-area top-left gui-box:x) block-width)
     (+ (~> *pong-game* game-area top-left gui-box:y)
        (-
         (~> player-human pad-y )
         (/ (pad-height player-human) 2))
        )
     block-width
     (~> player-human pad-height)))
  (cairo:fill-path)

  (gui-window:set-rgba "red")
  (let ((block-width 30))
    (cairo:rectangle
     (- (~> *pong-game* game-area top-left gui-box:x) block-width)
     (+ (~> *pong-game* game-area top-left gui-box:y)
        (- (~> player-human pad-y ) 2))
     (/ block-width 2)
     4))
  (cairo:fill-path)

  (warn "implement render player human"))

(defmethod render ((player-human player-computer))
  (gui-window:set-rgba "green")

  (let ((block-width 25))
    (cairo:rectangle
     (~> *pong-game* game-area bottom-right gui-box:x)
     (+ (~> *pong-game* game-area top-left gui-box:y)
        (-
         (~> player-human pad-y )
         (/ (pad-height player-human) 2))
        )
     block-width
     (~> player-human pad-height)))
  (cairo:fill-path)

  (gui-window:set-rgba "orange")
  (let ((block-width 30))
    (cairo:rectangle
     (+ (~> *pong-game* game-area bottom-right gui-box:x) (/ block-width 2))
     (+ (~> *pong-game* game-area top-left gui-box:y)
        (- (~> player-human pad-y ) 2))
     (/ block-width 2)
     4))
  (cairo:fill-path)

  (warn "implement render player computer"))

(defmethod render ((pong-game pong-game))
  (cairo:set-source-rgb  1 1 1)
  (cairo:paint)

  (let ((tb (make-instance 'gui-box:text-box
                           :top-left (make-instance 'gui-box:coordinates :x 20 :y 2)
                           :width 50
                           :height 20
                           :text "Pong will go here")))
    (~> pong-game pong-window (gui-window:add-child _ tb))
    (when nil
      (warn "going to render tb ~S ~S ~S"
            tb
            (~> tb gui-box::root-window gui-window:dimensions)
            (gui-window:current-motion gui-window:*lisp-app*)))
    (~> tb gui-box::mouse-overp)

    (render tb))

  (render (game-area pong-game))
  (render (ball pong-game))

  (render (player-human    pong-game))
  (render (player-computer pong-game)))

(defmethod render ((text-box gui-box:text-box))
  (cairo:select-font-face "Ubuntu Mono" :normal :bold)
  (cairo:set-font-size 20)

  (cairo:move-to 0 0)
  (let ((my-text (gui-box:text text-box)))
    (multiple-value-bind  (xb yb width height)
        (cairo:text-extents my-text)
      (declare (ignore xb yb
                       ;; width height
                       ))
      (setf (~> text-box gui-box:width)  width
            (~> text-box gui-box:height) height)

      (progn
        (gui-window:set-rgba (if (gui-box::mouse-overp text-box)
                                 (if (zerop (~> gui-window:*lisp-app* gui-window:mouse-button))
                                     "#ff000088"
                                     "green")
                                 "#0000ff88"))
        (cairo:rectangle
         (~> text-box gui-box:top-left gui-box:x)
         (~> text-box gui-box:top-left gui-box:y)
         (~> text-box gui-box:width)
         (~> text-box gui-box:height))
        (cairo:fill-path))


      (cairo:set-source-rgb 0 0 0)
      (cairo:move-to (~> text-box gui-box:top-left gui-box:x)
                     (+ height
                        (- 0 3)
                        (~> text-box gui-box:top-left gui-box:y)))
      (cairo:show-text (format nil "~A" my-text)))))

(defmethod render ((anything T))
  (warn "@@@ handling weird render @@@ ~S" (list (type-of anything) anything))
  (break "zzzzzzzzzzzzzzzzzzzz ~A" anything))

;;; === menu declaration =======================================================

(defun menu-bar (app lisp-window)
  (let ((menu (gio:make-menu)))
    (gui-menu:build-menu
     menu

     (gui-menu:prepare-submenu
      "File"
      (gui-menu:prepare-section
       nil
       (gui-menu:build-items
        (gui-menu:prepare-item-simple lisp-window app menu "Restart" "restart")))
      (gui-menu:prepare-section
       nil
       (gui-menu:build-items
        (gui-menu:prepare-item-simple lisp-window app menu "Quit" "quit")))
      ;; end of prepare-submenu File
      )

     (gui-menu:prepare-submenu
      "Help"
      (gui-menu:prepare-section
       nil
       (gui-menu:build-items
        (gui-menu:prepare-item-simple lisp-window app menu "Tutorial" "tutorial")))
      (gui-menu:prepare-section
       nil
       (gui-menu:build-items
        (gui-menu:prepare-item-simple lisp-window app menu "About" "about")))
      ;; end of prepare-submenu Help
      )
     ;; end of build-menu
     )

    (values menu)))

;;; === drawing ================================================================

(defmethod draw-window ((window pong-window))
  (if *pong-game*
      (~> *pong-game* render)
      (progn
        (cairo:set-source-rgb  1 1 1)
        (cairo:paint)

        (cairo:select-font-face "Ubuntu Mono" :normal :bold)
        (cairo:set-font-size 20)

        (let ((my-text "Press r to start"))
          (multiple-value-bind  (xb yb width height)
              (cairo:text-extents my-text)
            (declare (ignore xb yb width height)))
          (cairo:set-source-rgb 0 0 0)
          (cairo:move-to 20 20)
          (cairo:show-text (format nil "~A" my-text))))))

(defmethod draw-window ((window tutorial-window))
  (cairo:set-source-rgb  1 1 1)
  (cairo:paint)

  (cairo:select-font-face "Ubuntu Mono" :normal :bold)
  (cairo:set-font-size 20)

  (let ((my-text "Keyboard Shortcuts"))
    (multiple-value-bind  (xb yb width height)
        (cairo:text-extents my-text)
      (declare (ignore xb yb width height)))
    (cairo:set-source-rgb 0 0 0)
    (cairo:move-to 20 20)
    (cairo:show-text (format nil "~A" my-text)))

  (cairo:select-font-face "Ubuntu Mono" :normal :bold)
  (cairo:set-font-size 10)

  (loop for line in (list "r - Restart"
                              "t - show Tutorial")
        for ln = 0 then (1+ ln)
        do

           (let ((line-text line))
             (multiple-value-bind  (xb yb width height)
                 (cairo:text-extents line-text)
               (declare (ignore xb yb width height)))
             (cairo:set-source-rgb 0 0 0)
             (cairo:move-to 20 (+ 50 (* ln 20)))
             (cairo:show-text (format nil "~A" line-text)))))

;;; === events =================================================================

(defmethod process-event ((lisp-window pong-window) event &rest args)
    (unless (member event '(:timeout :motion))
      (format t "~&going to process ~A ~A  "
              event
              (case event
                ((:focus-enter :focus-leave)
                 (gui-window:window-hkey lisp-window))
                (:key-pressed
                 (destructuring-bind ((letter name code mods)) args
                   (warn "pressed ~S" (list letter name code mods (gui-window:window-hkey lisp-window)))))
                (T args))))

  (case event
    (:timeout
     ;; do nothing yet
     )
    (:menu-simple
     (destructuring-bind ((menu-item)) args
       (warn "menu item ~s" menu-item)
       (cond
         ((equal menu-item "restart")
          ;; start the game
          (restart-helper lisp-window))

         ((equal menu-item "quit")
          (gui-window:close-all-windows-and-quit))

         ((equal menu-item "tutorial")
          (show-tutorial-helper))

         ((equal menu-item "about")
          (gui-window:present-about-dialog
           (list
            :authors (list "Jacek Podkanski")
            :website "https://github.com/bigos"
            :program-name "Pong"
            :comments "Nothing to say yet"
            :license "Public Domain"
            :system-information (format nil "~A" (uiop/os:implementation-identifier))
            :logo-icon-name "application-x-addon")))

         (T
          (warn "not processed event ~S ~S" event args)))))
    (:menu-bool (warn "not processed event ~S ~S" event args))
    (:menu-radio (warn "not processed event ~S ~S" event args))
    ((:motion :motion-enter) ; we use simple case with one window so we ignore the window argument
     (destructuring-bind ((x y)) args
       (warn "motioning ~S ~S" x y)
       (setf (gui-window:mouse-coordinates gui-window:*lisp-app*) (cons x y)
             (gui-window:current-motion    gui-window:*lisp-app*) lisp-window)
       (when *pong-game*
         (mouse-set-human-pad-y *pong-game* y))))
    (:motion-leave
     (setf (gui-window:mouse-coordinates gui-window:*lisp-app*) nil
           (gui-window:current-motion gui-window:*lisp-app*) nil))
    (:focus-enter)
    (:focus-leave)
    (:pressed
     ;; TODO find better way of finding mouse buttons state
     (destructuring-bind ((button x y)) args
       (declare (ignore x y))
       (incf (gui-window:mouse-button gui-window:*lisp-app*) (expt 2 button)))
     (warn "button after press ~S" (gui-window:mouse-button gui-window:*lisp-app*)))
    (:released
     (destructuring-bind ((button x y)) args
       (declare (ignore button x y))
       (setf (gui-window:mouse-button gui-window:*lisp-app*) 0))
     (warn "button after release ~S" (gui-window:mouse-button gui-window:*lisp-app*)))
    (:scroll)
    (:resize
     (destructuring-bind ((w h)) args
       (gui-window:window-resize w h lisp-window)
       (when *pong-game* (resize *pong-game* w h))))
    (:key-pressed
     (destructuring-bind ((entered key-name key-code mods)) args
       (format t "~&>>> key pressed ~S~%" (list entered key-name key-code mods))
       (cond

         ((equalp entered "a")
          (break "exammine ~S" gui-window:*lisp-app*))
         ((equalp entered "r")
          (restart-helper lisp-window))
         ((equalp entered "t")
          (show-tutorial-helper))
         (t (warn "undandled key press ~S" args)))))
    (otherwise
     (warn "not handled event ~S ~S" event args)))

  (maphash (lambda (key lwin)
             (declare (ignore key))
             (gui-window:redraw-canvas lwin))
           (gui-window:all-windows)))

(defmethod process-event ((lisp-window tutorial-window) event &rest args)
  (unless (member event '(:timeout :motion))
    (format t "~&going to process ~A ~A  "
            event
            (case event
              ((:focus-enter :focus-leave)
               (gui-window:window-hkey lisp-window))
              (:key-pressed
               (destructuring-bind ((letter name code mods)) args
                 (warn "pressed ~S" (list letter name code mods (gui-window:window-hkey lisp-window)))))
              (T args))))
  (case event
    (:timeout
     ;; do nothing yet
     )
    ((:motion :motion-enter) ; we use simple case with one window so we ignore the window argument
     (destructuring-bind ((x y)) args
       (setf (gui-window:mouse-coordinates gui-window:*lisp-app*) (cons x y)
             (gui-window:current-motion    gui-window:*lisp-app*) lisp-window)))
    (:motion-leave
     (setf (gui-window:mouse-coordinates gui-window:*lisp-app*) nil
           (gui-window:current-motion gui-window:*lisp-app*) nil))
    (:focus-enter)
    (:focus-leave)
    (:pressed)
    (:released)
    (:scroll)
    (:resize)
    (:key-pressed
     (destructuring-bind ((entered key-name key-code mods)) args
       (format t "~&>>> key pressed ~S~%" (list entered key-name key-code mods))))
    (otherwise
     (warn "not handled event ~S ~S" event args))))

;; (defmethod process-event (lisp-window event &rest args)
;;   (warn "@@@ handling weird event @@@ ~S" (list (type-of lisp-window) lisp-window event args) ))

;;; --- enent helpers ----------------------------------------------------------
(defmethod restart-helper ((pong-window pong-window))
  (warn "setting *pong-game* and staring game")
  (setf *pong-game* (make-instance 'pong-game :pong-window pong-window))
  (start-game *pong-game*))

(defun show-tutorial-helper ()
  (gui-window:window-creation-from-menu
   "Tutorial"
   nil                                  ; no menu
   (make-instance 'tutorial-window)))

;;; === main ===================================================================

(defun main ()
  (setf gui-window:*client-fn-draw-objects*  'pong::draw-window
        gui-events:*client-fn-process-event* 'pong::process-event
        gui-window:*client-fn-menu-bar*      'pong::menu-bar
        gui-window:*initial-window-width*    600
        gui-window:*initial-window-height*   400
        gui-window:*initial-title*           "Pong")

  (gui-window:window (make-instance 'pong-window)))

;; (main)
