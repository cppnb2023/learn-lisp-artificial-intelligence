(load "../generic.lisp")
(defpackage :new-hopfield
  (:use :common-lisp :generic))

(in-package :new-hopfield)

(defstruct pattern
  vector
  name)

(defclass hopfield ()
  ((patterns
    :initform nil
    :accessor patterns)
   (weight ;;用于内部计算权重，防止重复开销
    :initform nil)))

(defun find-pos-by-func (comparator seq)
  (cond
    ((not (typep seq 'sequence))
     (error ""))
    ((listp seq)
     (loop for e in (cdr seq)
	   with best = (car seq)
	   with best-index = 0
	   for i = 1 then (1+ i) do
	   (when (funcall comparator e best)
	     (setf best e
		   best-index i))
	   finally
	   (return-from find-pos-by-func
	     (values best-index best))))
    ((vectorp seq)
     (loop for i from 1 below (length seq)
	   with best = (aref seq 0)
	   with best-index = 0 do
	   (when (funcall comparator (aref seq i) best)
	     (setf best (aref seq i)
		   best-index i))
	   finally
	   (return-from find-pos-by-func
	     (values best-index best))))))

(defmethod initialize-instance :after
    ((network hopfield) &key patterns)
  (with-slots ((net-pat patterns) weight) network
    (loop for (vec name) in patterns
	  for len = 1 then (1+ len) do
	  (push (make-pattern
		 :vector (coerce vec 'simple-vector)
		 :name name)
		net-pat)
	  finally
	  (setf weight
		(make-array len)))))

(defmethod recall ((network hopfield) input &optional output)
  (with-slots (patterns weight) network
    (with-vector ((temp (length input)))
      (set-result temp)
      (softmax weight
	       (domap (:norm weight) ((:list pat patterns))
		      (v* input (slot-value pat 'vector))))
      (loop for pat in patterns
	    for w across weight
	    with output =
	    (if output
		(vset! output 0)
		(make-vector (length input)))
	    do
	    (v+! output
		 output
		 (vscale w (slot-value pat 'vector)))
	    finally
	    (return-from recall
	      (values
	       output
	       (nth (find-pos-by-func #'> weight)
		    patterns)))))))

(let ((network (make-instance
		'hopfield
		:patterns
		'((#(0.0 0.0 0.0 0.0 0.0 0.0 0.0 
		     0.0 0.0 0.0 0.1 0.1 0.0 0.0 
		     0.0 0.0 0.0 0.1 0.1 0.0 0.0 
		     0.0 0.0 0.1 0.0 0.0 0.1 0.0 
		     0.0 0.0 0.1 0.1 0.1 0.1 0.0 
		     0.0 0.1 0.0 0.0 0.0 0.0 0.1 
		     0.0 0.0 0.0 0.0 0.0 0.0 0.0 
		     0.0 0.0 0.0 0.0 0.0 0.0 0.0)
		   "A")
		  (#(0.0 0.0 0.0 0.0 0.0 0.0 0.0 
		     0.0 0.1 0.1 0.1 0.1 0.1 0.0 
		     0.0 0.1 0.0 0.0 0.0 0.0 0.1 
		     0.0 0.1 0.1 0.1 0.1 0.1 0.1 
		     0.0 0.1 0.0 0.0 0.0 0.0 0.1 
		     0.0 0.1 0.1 0.1 0.1 0.1 0.1 
		     0.0 0.0 0.0 0.0 0.0 0.0 0.0 
		     0.0 0.0 0.0 0.0 0.0 0.0 0.0)
		   "B")
		  (#(0.0 0.0 0.0 0.0 0.0 0.0 0.0 
		     0.0 0.0 0.0 0.1 0.1 0.1 0.0 
		     0.0 0.1 0.1 0.0 0.0 0.0 0.1 
		     0.0 0.1 0.0 0.0 0.0 0.0 0.0 
		     0.0 0.1 0.0 0.0 0.0 0.0 0.0 
		     0.0 0.0 0.1 0.1 0.1 0.1 0.1 
		     0.0 0.0 0.0 0.0 0.0 0.0 0.0 
		     0.0 0.0 0.0 0.0 0.0 0.0 0.0)
		   "C")
		  (#(0.0 0.0 0.0 0.0 0.0 0.0 0.0 
		     0.0 0.1 0.1 0.1 0.0 0.0 0.0 
		     0.0 0.0 0.0 0.1 0.0 0.0 0.0 
		     0.0 0.0 0.0 0.1 0.0 0.0 0.0 
		     0.0 0.0 0.0 0.1 0.0 0.0 0.0 
		     0.0 0.1 0.1 0.1 0.1 0.1 0.0 
		     0.0 0.0 0.0 0.0 0.0 0.0 0.0 
		     0.0 0.0 0.0 0.0 0.0 0.0 0.0)
		   "1")
		  ;;和老式hopfield一样的数据，只不过全都除以10了
		  ;;防止exp函数溢出
		  ))))
  (multiple-value-bind (out pat)
      (recall network #(0.0 0.0 0.0 0.0 0.0 0.0 0.0 
			0.0 0.1 0.1 0.1 0.1 0.1 0.0 
			0.0 0.1 0.0 0.1 0.0 0.0 0.1 
			0.0 0.1 0.1 0.1 0.1 0.1 0.1 
			0.0 0.1 0.0 0.0 0.0 0.0 0.1 
			0.0 0.1 0.1 0.1 0.1 0.1 0.1 
			0.0 0.0 0.0 0.0 0.0 0.0 0.0 
			0.1 0.0 0.0 0.0 0.0 0.0 0.0))
    (format t "~a~%~a~%" out pat)))
