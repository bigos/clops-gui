(declaim (optimize (speed 0) (safety 2) (debug 3)))

;; (load "~/Programming/Lisp/clops-gui/examples/todo-list.lisp")

;;; === load ===================================================================
(push #p "~/Programming/Lisp/clops-gui/" ql:*local-project-directories*)
(ql:quickload '(:clops-gui) :silent nil)

;;; === package ================================================================
(defpackage #:todo-list
  (:use #:cl)
  (:import-from :serapeum
                :~>)
  (:import-from :defclass-std
                :defclass/std))

(in-package #:todo-list)

;;; === utilities ==============================================================
(defun slot-names (obj)
  (~> obj
      class-of
      sb-mop:class-slots
      (mapcar #'sb-mop:slot-definition-name  _)))

(defun slot-names-and-classes (obj)
  (loop for the-slot in (slot-names obj)
        collect (if (slot-boundp obj the-slot)
                    (cons the-slot (type-of (slot-value obj the-slot)))
                    the-slot)))

(defun slot-values-except (obj exceptions)
  (loop for the-slot in (slot-names obj)
        collect (if (slot-boundp obj the-slot)
                    (if (member the-slot exceptions)
                        (list the-slot
                              :ignored
                              (type-of (slot-value obj the-slot)))
                        (cons the-slot
                              (slot-value obj the-slot)))
                    the-slot)))

(defmethod print-object ((object gui-box:coordinates) stream)
  (print-unreadable-object (object stream :identity t :type t)
    (format stream "rel: ~Sx~S, abs: ~Sx~S"
            (gui-box:x object)
            (gui-box:y object)
            (gui-box:absolute-x object)
            (gui-box:absolute-y object))))

(defmethod print-object ((object gui-box:box) stream)
  (print-unreadable-object (object stream :identity t :type t)
    (format stream "obj: ~S"
            (slot-values-except object '(gui-box:parent
                                         gui-box:children)))))

(defun make-coordinates (x y)
    (make-instance 'gui-box:coordinates :x x :y y))

;;; === classes =================================================================
(defclass/std todo-window (gui-window:lisp-window)
  ((events)))

(defclass/std search-box (gui-box:box)
  ((text-field)
   (button)
 ))

(defclass/std todo-box   (gui-box:box)
  ((items :std 0)
   (last-clicked)))
(defclass/std todo-item   (gui-box:text-box) (()))
(defclass/std action-box (gui-box:box) (()))

(defclass/std action-add (gui-box:text-box)  (()))
(defclass/std action-remove (gui-box:text-box) (()))
(defclass/std action-up (gui-box:text-box)   (()))
(defclass/std action-down (gui-box:text-box) (()))

;;; === methods =================================================================
(defmethod initialize-instance :after ((window todo-window) &rest initargs &key)
  (declare (ignore initargs))

  (let
      ((search-box (make-instance 'search-box
                                  :parent window
                                  :top-left (make-coordinates 20 20)
                                  :width 400
                                  :height 50))
       (todo-box   (make-instance 'todo-box
                                  :parent window
                                  :top-left (make-coordinates 20 100)
                                  :width 400
                                  :height 200))
       (action-box (make-instance 'action-box
                                  :parent window
                                  :top-left (make-coordinates 20 330)
                                  :width 400
                                  :height 50)))

    (gui-window:add-child window search-box )
    (gui-box:add-child search-box
                       (make-instance 'gui-box:text-box
                                      :parent search-box
                                      :top-left (make-coordinates 10 10)
                                      :width 200
                                      :height 30))
    (gui-box:add-child search-box
                       (make-instance 'gui-box:text-box
                                      :parent search-box
                                      :top-left (make-coordinates 280 10)
                                      :width 110
                                      :height 30
                                      :text "Search"))

    (gui-window:add-child window todo-box   )

    (gui-window:add-child window action-box )
    (gui-box:add-child action-box (make-instance 'action-add
                                                 :parent action-box
                                                 :top-left (make-coordinates 5 5)
                                                 :width 20
                                                 :height 20
                                                 :text "Add"))
    (gui-box:add-child action-box (make-instance 'action-remove
                                                 :parent action-box
                                                 :top-left (make-coordinates 105 5)
                                                 :width 20
                                                 :height 20
                                                 :text "Remove"))
    (gui-box:add-child action-box (make-instance 'action-up
                                                 :parent action-box
                                                 :top-left (make-coordinates 205 5)
                                                 :width 20
                                                 :height 20
                                                 :text "Up"))
    (gui-box:add-child action-box (make-instance 'action-down
                                                 :parent action-box
                                                 :top-left (make-coordinates 305 5)
                                                 :width 20
                                                 :height 20
                                                 :text "Down"))))

(defmethod add-item ((window todo-window) string)
  (let ((todo-box (typed-widget window 'todo-box)))
    (gui-box:add-child todo-box (make-instance 'todo-item
                                               :parent todo-box
                                               :top-left (make-coordinates 4
                                                                           (+ (* (items todo-box) 40) 5) )
                                               :width 200
                                               :height 30
                                               :text (format nil "~a ~a"
                                                             string
                                                             (items todo-box))))
    (incf (items todo-box))))

(defmethod remove-item ((window todo-window))
  (let* ((todo-box (typed-widget window 'todo-box))
         (first-child (~> todo-box last-clicked)))
    (when first-child
      (gui-box:remove-child todo-box first-child)
      (decf (items todo-box)))))

;; (defmethod process-box ((box action-add))
;;   (warn "going to process action add ~S" box))
;;; the example below may be better

(defmethod process-box ((window todo-window) (box T))
  (warn "processing box")
  (typecase box
    (action-add
     (warn "processing action-add")
     (add-item window "Item 1"))
    (action-remove
     (warn "processing acion-remove")
     (remove-item window))
    (todo-item
     (warn "processing todo-item click ~S" box)
     (setf (~> box gui-box:parent last-clicked) box)
     )
    (t
     (warn "going to process unknown box ~S" (type-of box)))))

(defun text-dimentions (text size font slant weight)
  (cairo:select-font-face font slant weight)
  (cairo:set-font-size size)

  (multiple-value-bind (xb yb width height)
      (cairo:text-extents text)
    (declare (ignore xb yb))
    (cons width height)))

(defgeneric render (box)
    (:documentation "Render a BOX usinng cairo "))

(defmethod render ((search-box search-box))
  (cairo:select-font-face "Ubuntu Mono" :normal :bold)
  (cairo:set-font-size 12)

  (let* ((app gui-window:*lisp-app*)
         (my-text (format nil "~A - ~S"
                          "search box"
                          (gui-window:mouse-coordinates app))))
    (multiple-value-bind (xb yb width height)
        (cairo:text-extents "X")
      (declare (ignore xb yb width))

      (cairo:move-to    (~> search-box gui-box:top-left gui-box:absolute-x)
                        (+ (~> search-box gui-box:top-left gui-box:absolute-y) height)))
    (gui-window:set-rgba "red")
    (cairo:show-text my-text))

  (gui-window:set-rgba "#55667740")
  (cairo:rectangle
   (~> search-box gui-box:top-left gui-box:absolute-x)
   (~> search-box gui-box:top-left gui-box:absolute-y)
   (~> search-box gui-box:width)
   (~> search-box gui-box:height))
  (cairo:fill-path))

(defmethod render ((box gui-box:box))
  (gui-window:set-rgba "#ffff0040")
  (cairo:rectangle
   (~> box gui-box:top-left gui-box:absolute-x)
   (~> box gui-box:top-left gui-box:absolute-y)
   (~> box gui-box:width)
   (~> box gui-box:height))
  (cairo:fill-path))

(defmethod render :after ((box gui-box:box) )
  (loop for b in (gui-box:children box) do (render b)))

(defmethod render ((box gui-box:text-box))
  (gui-window:set-rgba (gui-box:box-color box))

  (let ((my-text (format nil "~A" (~> box gui-box:text))))
    (let ((the-text-dimentions (text-dimentions my-text 20 "Ubuntu Mono" :normal :bold)))
      (let ((width  (car the-text-dimentions))
            (height (cdr the-text-dimentions)))

        ;; some text boxes will not be resized on text change
        (gui-box:resize box width height)

        ;; draw a box with 4/2 pixels margin
        (cairo:rectangle
         (~> box gui-box:top-left gui-box:absolute-x)
         (~> box gui-box:top-left gui-box:absolute-y)
         (+ width 4)
         (+ height 4))
        (cairo:fill-path)

        ;; move cairo cursor relative to absolute box position to have the margin
        (cairo:move-to (~> box gui-box:top-left gui-box:absolute-x (+ _  2))
                       ;; so the height is useless here because I can not line up the -
                       (~> box gui-box:top-left gui-box:absolute-y (+ _ height 2)))
        (gui-window:set-rgba "black")
        (cairo:show-text my-text)))))

;;; === experiment ==============================================================
(defun experiment-first-window ()
  (setf gui-window:*client-fn-draw-objects*  'todo-list::draw-window)

  (setf gui-window:*lisp-app* (gui-window::make-lisp-app))
  (assert (zerop (hash-table-count (gui-window:all-windows))))

  (let ((lisp-window (make-instance 'todo-window)))
    (gui-window:window-creation-from-simulation :testing lisp-window)
    (assert (eq 1 (hash-table-count (gui-window:all-windows))))
    lisp-window))

;;; when you skip the arguments part of the function this may work as a forward
;;: declaration for process-event and hide the warning
(declaim (ftype function process-event))

(defun experiment ()
  "Experiment is for experimenting with processing events and drawing windows
  without the use of Gtk4 library. Instead of drawing on a window the outcomes
  of the events are drawn on *.png files."
  (warn "starting experiment")

  (let ((lisp-window (experiment-first-window))
        (events '((:RESIZE ((600 400))) (:KEY-RELEASED (("" "Return" 36 NIL)))
                  (:TIMEOUT (NIL)) (:MOTION-ENTER ((194.0d0 390.0d0)))
                  (:MOTION ((194.81414794921875d0 390.444091796875d0)))
                  (:MOTION-LEAVE (NIL)) (:MOTION-ENTER ((0.0d0 332.0d0)))
                  (:MOTION ((0.110321044921875d0 332.4322509765625d0))) (:TIMEOUT (NIL))
                  (:MOTION ((39.44886779785156d0 346.9728088378906d0)))
                  (:PRESSED ((1 39.44886779785156d0 346.9728088378906d0)))
                  (:RELEASED ((1 39.44886779785156d0 346.9728088378906d0)))
                  (:TIMEOUT (NIL)) (:KEY-PRESSED (("e" "e" 26 NIL))))))
    (loop for event in events
          for e = (car event)
          for eargs = (caadr event)
          do
             (break "data ~s" (list
                               gui-window:*lisp-app*
                               lisp-window))
             (funcall 'process-event
                      lisp-window
                      e
                      eargs ))))

(defun experiment2 ()
  "Experiment is for experimenting with processing events and drawing windows
  without the use of Gtk4 library. Instead of drawing on a window the outcomes
  of the events are drawn on *.png files."
  (warn "starting experiment")

  (let ((lisp-window (experiment-first-window))
        (events
          ' ((:RESIZE ((600 400))) (:TIMEOUT (NIL))
             (:MOTION-ENTER ((587.0d0 338.5d0)))
             (:MOTION ((587.1007690429688d0 338.9066162109375d0)))
             (:MOTION ((520.0015869140625d0 318.4851379394531d0)))
             (:MOTION ((442.2054748535156d0 310.7055358886719d0)))
             (:MOTION ((406.22479248046875d0 308.7606201171875d0)))
             (:MOTION ((400.39007568359375d0 308.7606201171875d0)))
             (:MOTION ((399.4816589355469d0 308.7606201171875d0)))
             (:MOTION ((385.2149963378906d0 308.7606201171875d0)))
             (:MOTION ((372.5731201171875d0 309.73309326171875d0)))
             (:MOTION ((346.3169250488281d0 309.73309326171875d0)))
             (:MOTION ((332.7026062011719d0 309.73309326171875d0)))
             (:MOTION ((303.529052734375d0 307.7881774902344d0)))
             (:MOTION ((264.6310119628906d0 305.8432922363281d0)))
             (:MOTION ((247.12689208984375d0 306.81573486328125d0)))
             (:MOTION ((218.92578125d0 309.73309326171875d0)))
             (:MOTION ((198.5043182373047d0 313.6228942871094d0)))
             (:MOTION ((190.72470092773438d0 314.5953369140625d0)))
             (:MOTION ((174.19302368164063d0 318.4851379394531d0)))
             (:MOTION ((168.3583221435547d0 320.4300537109375d0)))
             (:MOTION ((155.71644592285156d0 324.3198547363281d0)))
             (:MOTION ((145.0194854736328d0 328.20965576171875d0)))
             (:MOTION ((139.1847686767578d0 329.18212890625d0)))
             (:MOTION ((127.51536560058594d0 332.0994567871094d0)))
             (:MOTION ((114.87348937988281d0 335.9892883300781d0)))
             (:MOTION ((108.06632995605469d0 337.9341735839844d0)))
             (:MOTION ((94.45201110839844d0 341.823974609375d0)))
             (:MOTION ((81.81014251708984d0 345.7137756347656d0)))
             (:MOTION ((75.97543334960938d0 346.6862487792969d0)))
             (:MOTION ((66.25091552734375d0 349.6036071777344d0)))
             (:MOTION ((62.361114501953125d0 350.5760498046875d0)))
             (:MOTION ((54.581504821777344d0 350.5760498046875d0)))
             (:MOTION ((48.746795654296875d0 350.5760498046875d0)))
             (:MOTION ((46.80188751220703d0 350.5760498046875d0)))
             (:MOTION ((43.90443420410156d0 350.5760498046875d0)))
             (:MOTION ((42.193359375d0 350.5760498046875d0)))
             (:MOTION ((41.342002868652344d0 350.5760498046875d0)))
             (:MOTION ((39.635520935058594d0 350.5760498046875d0)))
             (:MOTION ((38.78386688232422d0 350.5760498046875d0)))
             (:MOTION ((37.07493591308594d0 350.5760498046875d0)))
             (:MOTION ((35.36961364746094d0 350.5760498046875d0))) (:TIMEOUT (NIL))
             (:MOTION ((34.75782775878906d0 349.9642639160156d0)))
             (:PRESSED ((1 34.75782775878906d0 349.9642639160156d0)))
             (:RELEASED ((1 34.75782775878906d0 349.9642639160156d0)))
             (:TIMEOUT (NIL))
             (:PRESSED ((1 34.75782775878906d0 349.9642639160156d0)))
             (:RELEASED ((1 34.75782775878906d0 349.9642639160156d0)))
             (:TIMEOUT (NIL)) (:TIMEOUT (NIL))
             (:PRESSED ((1 34.75782775878906d0 349.9642639160156d0)))
             (:RELEASED ((1 34.75782775878906d0 349.9642639160156d0)))
             (:TIMEOUT (NIL)) (:MOTION ((37.3184814453125d0 344.2027893066406d0)))
             (:MOTION ((41.16187286376953d0 336.5160217285156d0)))
             (:MOTION ((54.77619171142578d0 313.17718505859375d0)))
             (:MOTION ((59.638450622558594d0 306.3700256347656d0)))
             (:MOTION ((69.36296081542969d0 294.7005920410156d0)))
             (:MOTION ((79.08747863769531d0 284.97607421875d0)))
             (:MOTION ((84.92218017578125d0 279.1413879394531d0)))
             (:MOTION ((97.56405639648438d0 268.44439697265625d0)))
             (:MOTION ((104.3712158203125d0 263.5821533203125d0)))
             (:MOTION ((116.04063415527344d0 253.85763549804688d0)))
             (:MOTION ((127.71005249023438d0 246.07803344726563d0)))
             (:MOTION ((132.57229614257813d0 242.188232421875d0)))
             (:MOTION ((140.35191345214844d0 236.353515625d0)))
             (:MOTION ((146.18663024902344d0 232.46371459960938d0)))
             (:MOTION ((148.13153076171875d0 230.518798828125d0)))
             (:MOTION ((152.02133178710938d0 227.6014404296875d0)))
             (:MOTION ((153.9662322998047d0 226.62899780273438d0)))
             (:MOTION ((157.8560333251953d0 221.76675415039063d0)))
             (:MOTION ((159.8009490966797d0 213.98712158203125d0)))
             (:MOTION ((160.7733917236328d0 211.06976318359375d0)))
             (:MOTION ((162.71829223632813d0 203.2901611328125d0)))
             (:MOTION ((163.6907501220703d0 197.4554443359375d0)))
             (:MOTION ((163.6907501220703d0 194.53811645507813d0)))
             (:MOTION ((162.71829223632813d0 188.70339965820313d0)))
             (:MOTION ((161.745849609375d0 186.75848388671875d0)))
             (:MOTION ((159.8009490966797d0 182.86868286132813d0)))
             (:MOTION ((156.8835906982422d0 178.9788818359375d0)))
             (:MOTION ((154.93869018554688d0 176.0615234375d0))) (:TIMEOUT (NIL))
             (:MOTION ((152.02133178710938d0 172.17172241210938d0)))
             (:MOTION ((149.10397338867188d0 170.22682189941406d0)))
             (:MOTION ((147.15907287597656d0 169.25436401367188d0)))
             (:MOTION ((143.26927185058594d0 167.30946350097656d0)))
             (:MOTION ((139.3794708251953d0 165.36456298828125d0)))
             (:MOTION ((137.43455505371094d0 165.36456298828125d0)))
             (:MOTION ((133.5447540283203d0 165.36456298828125d0)))
             (:MOTION ((132.5923309326172d0 165.36456298828125d0)))
             (:MOTION ((129.7347412109375d0 165.36456298828125d0)))
             (:MOTION ((128.02896118164063d0 165.36456298828125d0)))
             (:MOTION ((127.17559814453125d0 165.36456298828125d0)))
             (:MOTION ((125.46980285644531d0 165.36456298828125d0)))
             (:MOTION ((124.61711120605469d0 165.36456298828125d0)))
             (:MOTION ((122.90965270996094d0 165.36456298828125d0)))
             (:MOTION ((121.20368957519531d0 165.36456298828125d0)))
             (:MOTION ((120.35081481933594d0 165.36456298828125d0)))
             (:MOTION ((118.45942687988281d0 164.41856384277344d0)))
             (:MOTION ((116.75296020507813d0 164.41856384277344d0)))
             (:MOTION ((115.90170288085938d0 164.41856384277344d0)))
             (:MOTION ((115.22586059570313d0 164.41856384277344d0)))
             (:MOTION ((114.68309020996094d0 164.41856384277344d0)))
             (:MOTION ((114.095703125d0 164.41856384277344d0)))
             (:MOTION ((113.51435852050781d0 163.83721923828125d0)))
             (:MOTION ((112.701171875d0 163.02403259277344d0)))
             (:MOTION ((110.75625610351563d0 162.05157470703125d0)))
             (:MOTION ((109.7838134765625d0 161.07913208007813d0)))
             (:MOTION ((107.83891296386719d0 159.1342315673828d0)))
             (:MOTION ((105.89401245117188d0 157.18931579589844d0)))
             (:MOTION ((104.92155456542969d0 156.2168731689453d0)))
             (:MOTION ((102.97665405273438d0 154.27197265625d0)))
             (:MOTION ((101.17796325683594d0 154.27197265625d0)))
             (:MOTION ((100.3271484375d0 154.27197265625d0)))
             (:MOTION ((98.6185302734375d0 154.27197265625d0)))
             (:MOTION ((97.76454162597656d0 154.27197265625d0))) (:TIMEOUT (NIL))
             (:PRESSED ((1 97.76454162597656d0 154.27197265625d0)))
             (:RELEASED ((1 97.76454162597656d0 154.27197265625d0)))
             (:MOTION ((101.5753173828125d0 163.79891967773438d0)))
             (:MOTION ((105.46513366699219d0 171.57852172851563d0)))
             (:MOTION ((121.99679565429688d0 192.97247314453125d0)))
             (:MOTION ((135.61111450195313d0 206.5867919921875d0)))
             (:MOTION ((143.39073181152344d0 215.33883666992188d0)))
             (:MOTION ((155.06015014648438d0 229.92559814453125d0)))
             (:MOTION ((158.949951171875d0 236.7327880859375d0)))
             (:MOTION ((164.78466796875d0 247.42974853515625d0)))
             (:MOTION ((168.67446899414063d0 256.1817932128906d0)))
             (:MOTION ((170.61936950683594d0 260.07159423828125d0)))
             (:MOTION ((173.53672790527344d0 268.82366943359375d0)))
             (:MOTION ((175.48162841796875d0 279.5206298828125d0)))
             (:MOTION ((178.39898681640625d0 290.21759033203125d0)))
             (:MOTION ((178.39898681640625d0 297.0247497558594d0)))
             (:MOTION ((178.39898681640625d0 300.8463134765625d0)))
             (:MOTION ((178.39898681640625d0 304.7009582519531d0)))
             (:MOTION ((178.39898681640625d0 308.59075927734375d0)))
             (:MOTION ((178.39898681640625d0 311.50811767578125d0)))
             (:MOTION ((178.39898681640625d0 316.3703918457031d0)))
             (:MOTION ((178.39898681640625d0 321.2326354980469d0)))
             (:MOTION ((178.39898681640625d0 323.17755126953125d0)))
             (:MOTION ((178.39898681640625d0 328.039794921875d0)))
             (:MOTION ((178.39898681640625d0 329.9847106933594d0)))
             (:MOTION ((177.42652893066406d0 333.87451171875d0)))
             (:MOTION ((175.48162841796875d0 339.7091979980469d0)))
             (:MOTION ((174.50918579101563d0 341.65411376953125d0)))
             (:TIMEOUT (NIL)) (:MOTION ((172.56427001953125d0 345.5439147949219d0)))
             (:MOTION ((170.61936950683594d0 347.48883056640625d0)))
             (:MOTION ((170.61936950683594d0 348.4334411621094d0)))
             (:MOTION ((170.61936950683594d0 350.1391906738281d0)))
             (:MOTION ((170.02243041992188d0 350.1391906738281d0)))
             (:MOTION ((169.42337036132813d0 350.1391906738281d0)))
             (:PRESSED ((1 169.42337036132813d0 350.1391906738281d0)))
             (:TIMEOUT (NIL))
             (:RELEASED ((1 169.42337036132813d0 350.1391906738281d0)))
             (:TIMEOUT (NIL))
             (:MOTION ((175.02227783203125d0 343.60711669921875d0)))
             (:MOTION ((186.6916961669922d0 331.93768310546875d0)))
             (:MOTION ((191.553955078125d0 327.075439453125d0)))
             (:MOTION ((200.30601501464844d0 316.37847900390625d0)))
             (:MOTION ((208.08563232421875d0 308.5988464355469d0)))
             (:MOTION ((212.9478759765625d0 304.70904541015625d0)))
             (:MOTION ((223.64486694335938d0 295.9570007324219d0)))
             (:MOTION ((238.23162841796875d0 285.2600402832031d0)))
             (:MOTION ((245.03878784179688d0 281.3702087402344d0)))
             (:MOTION ((256.70819091796875d0 273.5906066894531d0)))
             (:MOTION ((262.54290771484375d0 269.7008056640625d0)))
             (:MOTION ((272.2674255371094d0 263.8660888671875d0)))
             (:MOTION ((279.0745849609375d0 259.9762878417969d0)))
             (:MOTION ((281.991943359375d0 259.00384521484375d0)))
             (:MOTION ((285.8817443847656d0 257.0589294433594d0)))
             (:MOTION ((288.7991027832031d0 255.11404418945313d0)))
             (:MOTION ((289.74334716796875d0 255.11404418945313d0)))
             (:MOTION ((291.45330810546875d0 255.11404418945313d0)))
             (:MOTION ((294.3432922363281d0 253.19650268554688d0)))
             (:MOTION ((296.2882080078125d0 252.22406005859375d0)))
             (:MOTION ((300.1780090332031d0 252.22406005859375d0)))
             (:MOTION ((301.1299743652344d0 252.22406005859375d0)))
             (:MOTION ((302.83416748046875d0 252.22406005859375d0)))
             (:MOTION ((304.5468444824219d0 252.22406005859375d0)))
             (:MOTION ((305.39666748046875d0 252.22406005859375d0)))
             (:MOTION ((306.0331115722656d0 251.58761596679688d0)))
             (:MOTION ((306.7691345214844d0 250.85159301757813d0))) (:TIMEOUT (NIL))
             (:KEY-PRESSED (("e" "e" 26 NIL))))


            ))
    (loop for event in events
          for e = (car event)
          for eargs = (caadr event)
          do

             (funcall 'process-event
                      lisp-window
                      e
                      eargs ))))

;;; ============================================================================
(defun typed-widget (window widget-type)
  (first
   (loop for w in (gui-window:children window) when (typep w widget-type) collect w)))

(defmethod draw-window ((window todo-window))
  "Calls render for topmost boxes of the window."
  (let ((cv 0.13)) (cairo:set-source-rgb  cv cv cv))
  (cairo:paint)

  (render (typed-widget window 'search-box))
  (render (typed-widget window 'todo-box))
  (render (typed-widget window 'action-box))

  (let ((app gui-window:*lisp-app*))
    (when (and (eq (gui-window:current-motion app)
                   window)
               (gui-window:mouse-coordinates app))
      (gui-window:set-rgba "pink")
      (cairo:rectangle
       (car (gui-window:mouse-coordinates app))
       (cdr (gui-window:mouse-coordinates app))
       25
       25)
      (cairo:fill-path))))

(defmethod process-event ((lisp-window todo-window) event &rest args)
  (push (list event args) (events lisp-window))

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
    (:pressed
     (destructuring-bind ((button x y)) args
       (declare (ignore button))
       (let ((current-widget (gui-window:most-current-widget lisp-window)))
         (format t "~&processing mouse at ~S ~S ~%on widget ~S~%" x y
                 current-widget)
         (process-box lisp-window current-widget))))

    (:released)
    (:scroll)
    (:resize
     (destructuring-bind ((w h)) args
       (gui-window:window-resize w h lisp-window)))
    (:key-pressed
     (destructuring-bind ((entered key-name key-code mods)) args
       (format t "~&>>> key pressed ~S~%" (list entered key-name key-code mods))
       (cond
         ((equal entered "p")
          (break "break to examine ~S" lisp-window))
         ((equal entered "e")
          (format t "events ~S" (reverse (events lisp-window))))
         (t nil))))
    (otherwise
     (warn "not handled event ~S ~S" event args)))

  ;; add drawing ------------------------------

  ;; (maphash (lambda (key lwin)
  ;;            (declare (ignore key))
  ;;            (gui-window:redraw-canvas lwin))
  ;;          (gui-window:all-windows))

  (gui-window:redraw-canvas lisp-window (format  nil "~A" event)))

;;; === main ===================================================================
(defun init ()
  (setf
   ;; *model*  (make-instance 'model)
   ))

(defun main ()
  (init)

  (setf gui-window:*client-fn-menu-bar*      nil
        gui-window:*client-fn-draw-objects*  'todo-list::draw-window
        gui-events:*client-fn-process-event* 'todo-list::process-event
        gui-window:*initial-window-width*    600
        gui-window:*initial-window-height*   400
        gui-window:*initial-title*           "To-Do List")

  (gui-window:window (make-instance 'todo-window)))

;; (main)
