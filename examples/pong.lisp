(declaim (optimize (speed 0) (safety 3) (debug 3)))

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
   gui-drawing:*client-fn-draw-objects*  'pong::draw-window
   gui-app:*lisp-app* (gui-app:make-lisp-app))

  (assert (zerop (hash-table-count (gui-app:all-windows))))
  (let ((lisp-window (make-instance 'pong-window)))
    (gui-window-gtk:window-creation-from-simulation :testing lisp-window)
    (assert (eq 1 (hash-table-count (gui-app:all-windows))))
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

(defclass/std ball ()
  ((coordinates)
   (radius)
   (xd :std 0)
   (yd :std 0)))

(defclass/std game-area ()
  ((top-left)
   (bottom-right)))

(defclass/std pong-game ()
  ((player-human    :a :std (make-instance 'player-human))
   (player-computer :a :std (make-instance 'player-computer))
   (ball            :a :std (make-instance 'ball))
   (game-area       :a :std (make-instance 'game-area))
   (pong-window)
   (state :std :initial :documentation "Game state is: init, playing or won")))

;;; === methods ================================================================
(defmethod initialize-instance :after ((pong-game pong-game) &rest initargs &key)
  (declare (ignore initargs))
  (warn "running after initialize instance")

  (let* ((top-line  30)
         (left-line 30)
         (right-line  (- gui-window-gtk:*initial-window-width*  30))
         (bottom-line (- gui-window-gtk:*initial-window-height* (* 2 30))))
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
                                                :radius 5
                                                :xd 2.1
                                                :yd 0.5)
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

(let ((timer-count 0))
  (defmethod algor-set-computer-pad-y ((pong-game pong-game))
    (incf timer-count)
    (when (> timer-count 25) ;affects probability of computer losing
      (setf timer-count 0)

      (let ((ball-y (~> pong-game ball coordinates gui-box:y))
            (py     (~> pong-game player-computer pad-y))
            (state1 (make-random-state)))
        (let ((rrr (random 18 state1))) ;affects probability of computer losing

          (when (null py) (break "checking py ~S" py))

          (setf (~> pong-game player-computer pad-y) (or
                                                      (cond ((> ball-y py)
                                                             (+ py rrr))
                                                            ((< ball-y py)
                                                             (- py rrr))
                                                            ((eql ball-y py)
                                                             (if (oddp rrr)
                                                                 (- py rrr)
                                                                 (+ py rrr))))
                                                      ball-y)))))))

(defun posme (n)
  (if (> n 0)
      n
      (abs n)))
(defun negme (n)
  (if (< n 0)
      n
      (- 0 n)))

(defmethod move ((ball ball))
  (let ((radius (radius ball))
        (ga (~> *pong-game* game-area))
        (humpy (~> *pong-game* player-human pad-y))
        (compy (~> *pong-game* player-computer pad-y))
        (offset (~> *pong-game* game-area top-left gui-box:y))
        (half-pad (/ (~> *pong-game* player-human pad-height) 2)))
    (let ((x (~> ball coordinates gui-box:x))
          (y (~> ball coordinates gui-box:y))
          (wr (- (~> ga bottom-right gui-box:x) radius))
          (wl (+ (~> ga top-left     gui-box:x) radius))
          (wt (+ (~> ga top-left     gui-box:y) radius))
          (wb (- (~> ga bottom-right gui-box:y) radius)))

      (cond ((and (>= x wr)
                  (>= (+ (+ compy half-pad) offset)
                      y
                      (+ (- compy half-pad) offset)))
             (setf (xd ball) (negme (xd ball))))
            ((and (>= x (+ wr 10)))
             (setf (~> *pong-game* state) :won))
            ((and (<= x wl)
                  (>= (+ (+ humpy half-pad) offset)
                      y
                      (+ (- humpy half-pad) offset)))
             (setf (xd ball) (posme (xd ball))))
            ((and (<= x (- wl 10)))
             (setf (~> *pong-game* state) :won))
            ((<= y wt)
             (setf (yd ball) (posme (yd ball))))
            ((>= y wb)
             (setf (yd ball) (negme (yd ball))))
            (t nil))))
  (incf (~> ball coordinates gui-box:x) (xd ball))
  (incf (~> ball coordinates gui-box:y) (yd ball)))

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
  (cairo:fill-path))

(defmethod render ((player-computer player-computer))
  (gui-window:set-rgba "green")

  (let ((block-width 25))
    (cairo:rectangle
     (~> *pong-game* game-area bottom-right gui-box:x)
     (+ (~> *pong-game* game-area top-left gui-box:y)
        (-
         (~> player-computer pad-y )
         (/ (pad-height player-computer) 2))
        )
     block-width
     (~> player-computer pad-height)))
  (cairo:fill-path)

  (gui-window:set-rgba "orange")
  (let ((block-width 30))
    (cairo:rectangle
     (+ (~> *pong-game* game-area bottom-right gui-box:x) (/ block-width 2))
     (+ (~> *pong-game* game-area top-left gui-box:y)
        (- (~> player-computer pad-y ) 2))
     (/ block-width 2)
     4))
  (cairo:fill-path))

(defmethod render ((pong-game pong-game))
  (cairo:set-source-rgb  1 1 1)
  (cairo:paint)

  (let ((tb (make-instance 'gui-box:text-box
                           :top-left (make-instance 'gui-box:coordinates
                                                    :x (~> pong-game game-area top-left gui-box:x)
                                                    :y 4)
                           :width 50
                           :height 20
                           :text (ecase (~> pong-game state)
                                   (:initial  "Press p key to start")
                                   (:playing  "Move mouse to control left pad")
                                   (:won      "Press r key to restart")))))
    (~> pong-game pong-window (gui-window:add-child _ tb))
    (when nil
      (warn "going to render tb ~S ~S ~S"
            tb
            (~> tb gui-box:root-window gui-window:dimensions)
            (gui-app:current-motion gui-app:*lisp-app*)))
    (~> tb gui-box:mouse-overp)
    (render tb))

  (let ((tb (make-instance 'gui-box:text-box
                           :top-left (make-instance 'gui-box:coordinates :x 360 :y 5)
                           :width 50
                           :height 20
                           :text (format nil "~A" (state pong-game)))))
    (~> pong-game pong-window (gui-window:add-child _ tb))
    (when nil
      (warn "going to render tb ~S ~S ~S"
            tb
            (~> tb gui-box:root-window gui-window:dimensions)
            (gui-app:current-motion gui-app:*lisp-app*)))
    (~> tb gui-box:mouse-overp)
    (render tb))

  (render (game-area pong-game))
  (render (ball pong-game))

  (render (player-human    pong-game))
  (render (player-computer pong-game)))

(defmethod render ((text-box gui-box:text-box))
  (cairo:select-font-face "Ubuntu Mono" :normal :bold)
  (cairo:set-font-size 20)

  (cairo:move-to 0 0)
  (let ((my-text (format nil "~A"  (gui-box:text text-box))))
    (multiple-value-bind  (xb yb width height)
        (cairo:text-extents my-text)
      (declare (ignore xb yb
                       ;; width height
                       ))
      (setf (~> text-box gui-box:width)  (+ 2 width)
            (~> text-box gui-box:height) (+ 2 height))

      (progn
        (gui-window:set-rgba (gui-box:box-color text-box))

        (cairo:rectangle
         (~> text-box gui-box:top-left gui-box:x)
         (~> text-box gui-box:top-left gui-box:y)
         (~> text-box gui-box:width)
         (~> text-box gui-box:height))
        (cairo:fill-path))


      (cairo:set-source-rgb 0 0 0)
      (cairo:move-to (~> text-box gui-box:top-left gui-box:x)
                     (+ height
                        (~> text-box gui-box:top-left gui-box:y)))
      (cairo:show-text my-text))))

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
     (when (and *pong-game* (eql :playing (state *pong-game*)))
       (~> *pong-game* ball move)
       (~> *pong-game* algor-set-computer-pad-y )))
    (:menu-simple
     (destructuring-bind ((menu-item)) args
       (warn "menu item ~s" menu-item)
       (cond
         ((equal menu-item "restart")
          ;; start the game
          (restart-helper lisp-window))

         ((equal menu-item "quit")
          (gui-window-gtk:close-all-windows-and-quit))

         ((equal menu-item "tutorial")
          (show-tutorial-helper))

         ((equal menu-item "about")
          (gui-window-gtk:present-about-dialog
           (list
            :authors (list "Jacek Podkanski")
            :website "https://github.com/bigos/clops-gui/blob/master/examples/pong.lisp"
            :program-name "Pong"
            :comments "Pong example written for cl-gtk4"
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
       (setf (gui-app:mouse-coordinates gui-app:*lisp-app*) (cons x y)
             (gui-app:current-motion    gui-app:*lisp-app*) lisp-window)
       (when *pong-game*
         (mouse-set-human-pad-y *pong-game* y))))
    (:motion-leave
     (setf (gui-app:mouse-coordinates gui-app:*lisp-app*) nil
           (gui-app:current-motion gui-app:*lisp-app*) nil))
    (:focus-enter)
    (:focus-leave)
    (:pressed
     ;; TODO find better way of finding mouse buttons state
     (destructuring-bind ((button x y)) args
       (declare (ignore x y))
       (incf (gui-app:mouse-button gui-app:*lisp-app*) (expt 2 button)))
     (warn "button after press ~S" (gui-app:mouse-button gui-app:*lisp-app*)))
    (:released
     (destructuring-bind ((button x y)) args
       (declare (ignore button x y))
       (setf (gui-app:mouse-button gui-app:*lisp-app*) 0))
     (warn "button after release ~S" (gui-app:mouse-button gui-app:*lisp-app*)))
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
          (break "exammine ~S" gui-app:*lisp-app*))
         ((equalp entered "r")
          (restart-helper lisp-window))
         ((equalp entered "p")
          (start-playing-helper lisp-window))
         ((equalp entered "t")
          (show-tutorial-helper))
         (t (warn "undandled key press ~S" args)))))
    (otherwise
     (warn "not handled event ~S ~S" event args)))

  (maphash (lambda (key lwin)
             (declare (ignore key))
             (gui-window:redraw-canvas lwin))
           (gui-app:all-windows)))

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
       (setf (gui-app:mouse-coordinates gui-app:*lisp-app*) (cons x y)
             (gui-app:current-motion    gui-app:*lisp-app*) lisp-window)))
    (:motion-leave
     (setf (gui-app:mouse-coordinates gui-app:*lisp-app*) nil
           (gui-app:current-motion gui-app:*lisp-app*) nil))
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

(defmethod start-playing-helper ((pong-window pong-window))
  (warn "setting *pong-game* and staring game")
  (setf (state *pong-game*) :playing))

(defun show-tutorial-helper ()
  (gui-window-gtk:window-creation-from-menu
   "Tutorial"
   nil                                  ; no menu
   (make-instance 'tutorial-window)))

;;; === main ===================================================================

(defun main ()
  (setf gui-drawing:*client-fn-draw-objects*  'pong::draw-window
        gui-events:*client-fn-process-event* 'pong::process-event
        gui-window-gtk:*client-fn-menu-bar*      'pong::menu-bar
        gui-window-gtk:*initial-window-width*    600
        gui-window-gtk:*initial-window-height*   400
        gui-window-gtk:*timeout-period*          (/ 1000 100)
        gui-window-gtk:*initial-title*           "Pong")

  (gui-window-gtk:window (make-instance 'pong-window)))

;; (main)
