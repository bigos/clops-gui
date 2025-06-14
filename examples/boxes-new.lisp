(declaim (optimize (speed 0) (safety 3) (debug 3)))

;;;; Example of boxes new

;;; load ===================================================================
;; (load "~/Programming/Lisp/clops-gui/examples/boxes-new.lisp")

(ql:quickload '(:clops-gui) :silent nil)

;;; package ================================================================
(defpackage #:boxes-new
  (:use #:cl)
  (:import-from :serapeum
   :~>)
  (:import-from :defclass-std
   :defclass/std))

(in-package #:boxes-new)
