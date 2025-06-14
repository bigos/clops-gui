;;; type related macros

(defpackage :bigos-macros
  (:use :cl)
  (:import-from :defclass-std :defclass/std))

(in-package :bigos-macros)

(defmacro decft (fn-name argument-types return-type)
  `(declaim (ftype (function ,argument-types ,return-type) ,fn-name)))


(defmacro with-base-defclass (base-class inheritance-list slots &rest child-classes)
  `(progn
     (defclass/std ,base-class ,inheritance-list ,slots)
     ,@ (loop for c in child-classes
              collect
              (if (atom c)
                  `(defclass/std ,c       ,(list base-class) ())
                  `(defclass/std ,(car c) ,(list base-class) ,(cadr c))))))
