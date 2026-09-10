;;; type related macros


(in-package :bigos-macros)

(defmacro once-only ((&rest names) &body body)
  (let ((gensyms (loop for n in names collect (gensym (string-upcase (symbol-name n))))))
    `(let (,@(loop for g in gensyms collect `(,g (gensym (string-upcase ,(symbol-name g))))))
       `(let (,,@(loop for g in gensyms for n in names collect ``(,,g ,,n)))
          ,(let (,@(loop for n in names for g in gensyms collect `(,n ,g)))
             ,@body)))))

(defmacro with-gensyms (syms &body body)
  `(let ,(mapcar #'(lambda (s)
                     `(,s (gensym ,(string-upcase (symbol-name s)))))
                 syms)
     ,@body))

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
