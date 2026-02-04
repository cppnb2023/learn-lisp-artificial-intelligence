(defpackage :hopfield
  (:use :common-lisp))

(in-package :hopfield)

(defstruct things
  pattern
  things-name)

(defclass hopfield-network ()
  ((things-list
    :initarg  :things-list
    :initform nil
    :accessor things-list)
   (weight
    :initform nil
    :accessor weight)
   (index-array
    :initform nil
    :accessor index-array)))

(defun clip(x)
  (if (> x 0) 1 -1))

(defun pattern= (a b)
  (let ((a-len (length a))
	(b-len (length b)))
    (when (= a-len b-len)
      (loop with res = t
	    for i from 0 below a-len do
	    (setf res (and res (= (aref a i)
				  (aref b i))))
	    finally (return res)))))

(defun shuffle (array)
  (loop for i from (1- (length array)) downto 1
	for j = (random (1+ i)) do
	(rotatef (aref array i) (aref array j))))

(defmethod initialize-instance :after
    ((hopfield-network hopfield-network) &key things-list)
  (with-slots ((things things-list)
	       (weight weight)
	       (index-array index-array)) hopfield-network
    (setf things nil)
    (let ((max-len
	   (loop for th in things-list
		 maximize (length (car th)) into res
		 do
		 (push (make-things :pattern (car th)
				    :things-name (cadr th))
		       things)
		 finally (return res))))
      (setf weight (make-array (list max-len max-len)
			       :element-type '(signed-byte 32)
			       :initial-element 0))
      (setf index-array
	    (make-array max-len
			:element-type '(unsigned-byte 32)))
      (loop for i from 0 below max-len do
	    (setf (aref index-array i) i))
      (loop for th in things
	    for pat = (slot-value th 'pattern)
	    for pat-len = (length pat) do
	    (progn
	      (map-into pat #'clip pat)
	      (loop for i from 1 below (length pat) do
		    (loop for j from 0 below i
			  for mul-res = (* (aref pat i)
					   (aref pat j))
			  do
			  (progn
			    (incf (aref weight i j) mul-res)
			    (incf (aref weight j i) mul-res)))))))))

(defmethod hopfield-recall
    ((hopfield-network hopfield-network) limit pattern)
  (with-accessors ((things-list things-list)
		   (weight weight)
		   (index-array index-array)) hopfield-network
    (map-into pattern #'clip pattern)
    (loop with count = 0
	  with pat-len = (length pattern)
	  while (< count limit) do
	  (progn
	    (setf count 0)
	    (shuffle index-array)
	    (loop for i from 0 below pat-len
		  for l = (aref index-array i)
		  for new-val =
		  (clip
		   (loop for r from 0 below pat-len
			 sum (* (aref pattern r)
				(aref weight  r l))))
		  do
		  (if (= new-val (aref pattern l))
		      (incf count)
		      (setf (aref pattern l) new-val)))))
    (find pattern things-list
	  :test #'(lambda (a b)
		    (pattern= a (slot-value b 'pattern))))))

(let ((network
       (make-instance 'hopfield-network
		      :things-list
		      '((#(0 0 0 0 0 0 0 
			   0 0 0 1 1 0 0 
			   0 0 0 1 1 0 0 
			   0 0 1 0 0 1 0 
			   0 0 1 1 1 1 0 
			   0 1 0 0 0 0 1 
			   0 0 0 0 0 0 0 
			   0 0 0 0 0 0 0 )
			 "A")
			(#(0 0 0 0 0 0 0 
			   0 1 1 1 1 1 0 
			   0 1 0 0 0 0 1 
			   0 1 1 1 1 1 1 
			   0 1 0 0 0 0 1 
			   0 1 1 1 1 1 1 
			   0 0 0 0 0 0 0 
			   0 0 0 0 0 0 0 )
			 "B")
			(#(0 0 0 0 0 0 0 
			   0 0 0 1 1 1 0 
			   0 1 1 0 0 0 1 
			   0 1 0 0 0 0 0 
			   0 1 0 0 0 0 0 
			   0 0 1 1 1 1 1 
			   0 0 0 0 0 0 0 
			   0 0 0 0 0 0 0 
			   )
			 "C")
			;;(#(0 0 0 0 0 0 0 
			;;   0 1 1 1 0 0 0 
			;;   0 0 0 1 0 0 0 
			;;   0 0 0 1 0 0 0 
			;;   0 0 0 1 0 0 0 
			;;   0 1 1 1 1 1 0 
			;;   0 0 0 0 0 0 0 
			;;   0 0 0 0 0 0 0 
			;;   )
			;; "1")
			;;1982年的霍普菲尔德网络能存的模式数量特别少
			;;并且相似度比较高的还会互相影响形成伪记忆
			))))
  (format t "~a~%" (hopfield-recall network 56
				    #(0 0 0 0 0 0 0 
				      0 1 1 1 1 1 0 
				      0 1 0 0 0 0 1 
				      0 1 1 1 1 1 1 
				      0 1 0 0 0 0 1 
				      0 1 1 1 1 1 1 
				      0 0 0 0 0 0 0 
				      0 0 0 0 0 0 0 ))))
