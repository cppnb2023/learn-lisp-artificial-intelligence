(defpackage :zxc-generic
  (:use :common-lisp)
  (:export :singlep :it :aif :awhen :aunless))

(in-package :zxc-generic)

(defun singlep (var)
  (and (consp var) (null (cdr var))))

(defmacro aif (cond then else)
  `(let ((it ,cond))
	  (if it ,then ,else)))

(defmacro awhen (cond &body then)
  `(aif ,cond (progn ,@then) nil))

(defmacro aunless (cond &body else)
  `(aif ,cond nil (progn ,@else)))

