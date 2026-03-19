(defpackage :zxc-matrix-template
  (:use :common-lisp :zxc-generic))

(in-package :zxc-matrix-template)

(defclass matrix-template () ())

(defgeneric rows (matrix))
(defgeneric cols (matrix))

(defgeneric matrix-add-into (result a b))
(defgeneric matrix-sub-into (result a b))
(defgeneric matrix-mul-into (result a b))

(defgeneric matrix-apply-into (result func a))
(defgeneric matrix-scalar-mul-into (result scalar a))
(defgeneric matrix-copy-into (result a))

(defmacro defmultiple-op (fname (result rest) &body body)
  `(defun ,fname (,result &rest ,rest)
	  (cond
		 ((null ,rest) ,result)
		 ((singlep ,rest)
		  (destructuring-bind (a) ,rest
			 (declare ())
			 ,@(aif (find :single body :key #'first)
					  (cdr it)
					  (list `(matrix-copy-into ,result a)))))
		 (t
		  (destructuring-bind (a b &rest ,rest) ,rest
				,@(cdr (find :more body :key #'first)))))))

(defmultiple-op matrix+ (result matrixs)
  (:more
	(matrix-add-into result a b)
	(dolist (m matrixs)
	  (matrix-add-into result result m))))

(defmultiple-op matrix- (result matrixs)
  (:single
	(matrix-scalar-mul-into result -1 a))
  (:more
	(matrix-sub-into result a b)
	(dolist (m matrixs)
	  (matrix-sub-into result result m))))

(defmultiple-op matrix* (result matrixs)
  (:more
	(matrix-mul-into result a b)
	(dolist (m matrixs)
	  (matrix-mul-into result result m))))
