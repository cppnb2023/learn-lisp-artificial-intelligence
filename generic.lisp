(defpackage :generic
  (:use :common-lisp)
  (:export
   :vector-copy
   :vector-add-into
   :vector-sub-into
   :vector-dot-mul
   :vector-num-mul-into
   :with-vector))

(in-package :generic)

(defun vector-copy (dst src)
  (if (= (length dst) (length src))
      (loop for i from 0 below (length src) do
	    (setf (aref dst i) (aref src i))
	    finally
	    (return dst))
      (error "dst length not eq src length")))

(defun binary-vector-add-into (res vec1 vec2)
  (if (= (length res) (length vec1) (length vec2))
      (loop for i from 0 below (length res) do
	    (setf (aref res i) (+ (aref vec1 i)
				  (aref vec2 i)))
	    finally
	    (return res))
      (error "")))

(defun binary-vector-sub-into (res vec1 vec2)
  (if (= (length res) (length vec1) (length vec2))
      (loop for i from 0 below (length res) do
	    (setf (aref res i) (- (aref vec1 i)
				  (aref vec2 i)))
	    finally
	    (return res))
      (error "")))

(defun vector-add-into (res &rest vectors)
  (vector-copy res (car vectors))
  (loop for v in (cdr vectors) do
	(binary-vector-add-into res res v)
	finally
	(return res)))

(defun vector-sub-into (res &rest vectors)
  (if (cdr vectors)
      (progn
	(vector-copy res (car vectors))
	(loop for v in (cdr vectors) do
	      (binary-vector-sub-into res res v)
	      finally
	      (return res)))
      (loop for i from 0 below (length res)
	    with v = (car vectors) do
	    (setf (aref res i) (- (aref v i)))
	    finally
	    (return res))))

(defun vector-dot-mul (vec1 vec2)
  (if (= (length vec1) (length vec2))
      (loop for i from 0 below (length vec1)
	    sum (* (aref vec1 i) (aref vec2 i)))
      (error "vec1 length not eq vec2 length")))

(defun vector-num-mul-into (res num vec1)
  (if (= (length res) (length vec1))
      (loop for i from 0 below (length vec1) do
	    (setf (aref res i) (* (aref vec1 i) num))
	    finally
	    (return res))
      (error "res length not eq vec1 length")))

(defmacro with-vector ((len sym) &body body)
  `(block nil
     (let ((_ (make-array ,len)))
       (macrolet ((v+ (&body body) `(vector-add-into _ ,@body))
		  (v* (&body body) `(vector-dot-mul ,@body))
		  (v- (&body body) `(vector-sub-into _ ,@body))
		  (vscale (res num vec)
		    `(vector-num-mul-into ,res ,num ,vec))
		  (vcopy (vec1 vec2)
		    `(vector-copy ,vec1 ,vec2)))
	 ,@body))))

