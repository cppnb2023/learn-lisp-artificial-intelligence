(defpackage :generic
  (:use :common-lisp)
  (:export
   :vector-copy
   :vector-add-into
   :vector-sub-into
   :vector-dot-mul
   :vector-num-mul-into
   :vector-apply
   :vector-map-into
   :vector-elements-set
   :vector-softmax-into
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
  (if (= (length vec1) (length vec2))
      (loop for i from 0 below (length res) do
	    (setf (aref res i) (+ (aref vec1 i)
				  (aref vec2 i)))
	    finally
	    (return res))
      (error "")))

(defun binary-vector-sub-into (res vec1 vec2)
  (if (= (length vec1) (length vec2))
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
  (loop for i from 0 below (length vec1) do
	(setf (aref res i) (* (aref vec1 i) num))
	finally
	(return res)))

(defun vector-elements-set (vec1 num)
  (loop for i from 0 below (length vec1) do
	(setf (aref vec1 i) num)))

(defun vector-apply (res vec func)
  (loop for i from 0 below (length vec) do
	(setf (aref res i) (funcall func (aref res i)))
	finally
	(return res)))

(defun vector-map-into (res func &rest vecs)
  (let ((len (length (car vecs))))
    (assert (loop for v in (cdr vecs)
		  always (= len (length v))) ()
	    "All vectors must have same length")
    (loop for i from 0 below len do
	  (setf (aref res i)
		(apply func (mapcar (lambda (v)
				      (aref v i))
				    vecs))))))

(defun vector-softmax-into (res vec &key (temperature 1.0))
  (let ((len (length vec)))
    (declare (type fixnum len))
    (loop for i from 0 below len do
	  (setf (aref res i) (/ (exp (aref vec i)) temperature)))
    (let ((sum (loop for i from 0 below len
		     sum (aref res i))))
      (declare (type single-float sum))
      (loop for i from 0 below len do
	    (setf (aref res i) (/ (aref res i) sum))))))

(defmacro vector-do-map (dst (&rest bindings) &body body)
  (let ((i-sym (gensym "i")))
    `(loop for ,i-sym from 0 below (length ,dst)
	   ,@(loop for (bind prep vec) in bindings append
		   `(for ,bind ,prep ,vec))
	   do
	   (setf (aref ,dst ,i-sym)
		 (progn
		   ,@body))
	   finally
	   (return ,dst))))

(defmacro with-vector ((&rest bindings) &body body)
  (let ((result-sym (gensym "result")))
    `(let (,@(loop for (sym len) in bindings collect
		   `(,sym
		     (make-array ,len
				 :element-type 'single-float)))
	   (,result-sym nil))
       ,@(loop for (sym len) in bindings collect
	       `(declare (type (simple-array single-float
					     ,@(when (numberp len)
						  `((,len))))
			       ,sym)))
	 (macrolet ((,(intern "V+") (res &body body)
		      `(vector-add-into ,res ,@body))
		    (,(intern "V*") (&body body)
		      `(vector-dot-mul ,@body))
		    (,(intern "V-") (res &body body)
		      `(vector-sub-into ,res ,@body))
		    (,(intern "VSCALE") (res num vec)
		      `(vector-num-mul-into ,res ,num ,vec))
		    (,(intern "VCOPY") (vec1 vec2)
		      `(vector-copy ,vec1 ,vec2))
		    (,(intern "VAPPLY") (res vec func)
		      `(vector-apply ,res ,vec ,func))
		    (,(intern "VMAP") (res func &body vecs)
		      `(vector-map-into ,res ,func ,@vecs))
		    (,(intern "VSET") (vec num)
		      `(vector-elements-set ,vec ,num))
		    (,(intern "MAKE-VECTOR") (len)
		      `(make-array ,len
				     :element-type 'single-float
				     :initial-element 0.0))
		    (,(intern "SOFTMAX") (res vec &optional (temperature 1.0))
		      `(vector-softmax-into ,res ,vec
					    :temperature ,temperature))
		    (,(intern "DOMAP") (dst (&rest bindings) &body body)
		      `(vector-do-map ,dst ,bindings ,@body)))
	   ,@body))))

