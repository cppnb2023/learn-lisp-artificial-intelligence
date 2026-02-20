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

(defun singlep (lst)
  (and (consp lst) (null (cdr lst))))

(defmacro vector-do-map ((key dst &rest others) (&rest bindings) &body body)
  (flet ((parse (binding idx-sym)
	   (ecase (first binding)
	     (:norm
	      (assert (= (length binding) 3) ()
		      "for :NORM BINDING need 3 arguments~% but have ~a"
		      (length binding))
	      `(for ,(second binding) = (aref ,(third binding) ,idx-sym)))
	     (:row
	      (assert (= (length binding) 4) ()
		      "for :ROW BINDING need 4 arguments~% but have ~a"
		      (length binding))
	      `(for ,(second binding) =
		(aref ,(third binding) ,(fourth binding) ,idx-sym)))
	     (:col
	      (assert (= (length binding) 4) ()
		      "for :COL BINDING need 4 arguments~% but have ~a"
		      (length binding))
	      `(for ,(second binding) =
		    (aref ,(third binding) ,idx-sym ,(fourth binding))))
	     (:list
	      (assert (<= 3 (length binding) 4) ()
		      "for :LIST BINDING need 3 or 4 arguments~% but have ~a"
		      (length binding))
	      `(for ,(second binding) in ,(third binding)
		    by ,(if (fourth binding) (fourth binding) #'cdr))))))
    (assert (symbolp key) ()
	    "KEY must be symbol")
    (let ((dst-sym (gensym "dst")))
      `(let ((,dst-sym ,dst))
	 (assert (arrayp ,dst-sym) ()
		 "dst must be array or vector:~%~a" ,dst-sym)
	 (loop for i from 0 below
	       ,(ecase key
		  (:norm
		   `(length ,dst-sym))
		  (:row
		   `(array-dimension ,dst-sym 1))
		  (:col
		   `(array-dimension ,dst-sym 0)))
	       ,@(loop for binding in bindings append
		       (parse binding 'i))
	       do
	       (setf ,(ecase key
			(:norm
			 (assert (null others) ()
				 "for :NORM OTHERS must be NIL")
			 `(aref ,dst-sym i))
			(:row
			 (assert (singlep others) ()
				 "for :ROW OTHERS only have one element")
			 `(aref ,dst-sym ,(car others) i))
			(:col
			 (assert (singlep others) ()
				 "for :COL OTHERS only have one element")
			 `(aref ,dst-sym i ,(car others))))
		     (progn
		       ,@body))
	       finally
	       (return ,dst-sym))))))

(defun make-dimension-accessor
    (array dimension &optional writep)
  (assert (arrayp array) ()
	  "~a not is array" array)
  (assert (integerp dimension) ()
	  "~a not is integer" dimension)
  (when (minusp dimension)
    (setf dimension (+ (array-rank array)
		       dimension)))
  (let* ((rank (array-rank array))
	 (total-len
	  (apply #'* (array-dimensions array)))
	 (step
	  (apply #'* (nthcdr (1+ dimension)
			     (array-dimensions array))))
	 (remap
	  (make-array total-len
		      :displaced-to array
		      :displaced-index-offset 0)))
    (declare (type fixnum total-len))
    (declare (type fixnum step))
    (declare (type fixnum rank))
    (setf step (if (zerop step) 1 step))
    (if writep
	(lambda (idx new-val)
	  (setf (aref remap (* idx step)) new-val))
	(lambda (idx)
	  (aref remap (* idx step))))))

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
       (flet ((,(intern "SET-RESULT") (new-res)
		(setf ,result-sym new-res))
	      (,(intern "READ-RESULT") () ,result-sym))
	 (macrolet
	     ((,(intern "V+!") (res &body body)
		`(vector-add-into ,res ,@body))
	      (,(intern "V*") (&body body)
		`(vector-dot-mul ,@body))
	      (,(intern "V-!") (res &body body)
		`(vector-sub-into ,res ,@body))
	      (,(intern "VSCALE!") (res num vec)
		`(vector-num-mul-into ,res ,num ,vec))
	      (,(intern "VCOPY!") (vec1 vec2)
		`(vector-copy ,vec1 ,vec2))
	      (,(intern "VAPPLY!") (res vec func)
		`(vector-apply ,res ,vec ,func))
	      (,(intern "VMAP!") (res func &body vecs)
		`(vector-map-into ,res ,func ,@vecs))
	      (,(intern "VSET!") (vec num)
		`(vector-elements-set ,vec ,num))
	      (,(intern "V+") (&body body)
		`(vector-add-into ,',result-sym ,@body))
	      (,(intern "V-") (&body body)
		`(vector-sub-into ,',result-sym ,@body))
	      (,(intern "VSCALE") (num vec)
		`(vector-num-mul-into ,',result-sym ,num ,vec))
	      (,(intern "VCOPY") (vec2)
		`(vector-copy ,',result-sym ,vec2))
	      (,(intern "VAPPLY") (vec func)
		`(vector-apply ,',result-sym ,vec ,func))
	      (,(intern "VMAP") (func &body vecs)
		`(vector-map-into ,',result-sym ,func ,@vecs))
	      (,(intern "VSET") (num)
		`(vector-elements-set ,',result-sym ,num))
	      (,(intern "MAKE-VECTOR") (len)
		`(make-array ,len
			     :element-type 'single-float
			     :initial-element 0.0))
	      (,(intern "SOFTMAX") (res vec
					&optional (temperature 1.0))
		`(vector-softmax-into ,res ,vec
				      :temperature ,temperature))
	      (,(intern "DOMAP") (&body body)
		`(vector-do-map ,@body))
	      (,(intern "PROJECT") (array dimension
					  &optional writep)
		`(make-dimension-accessor array dimension writep)))
	      ,@body)))))
