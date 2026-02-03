(defpackage :multiple
  (:use common-lisp))

(in-package :multiple)

(defun sigmoid (x)
  (/ 1.0 (+ 1.0 (exp (- x)))))

(defun d-sigmoid (x)
  (let ((y (sigmoid x)))
    (* y (- 1.0 y))))

(defun relu (x)
  (if (> x 0.0) x 0.0))

(defun d-relu (x &rest other)
  (if (> x 0.0) 1.0 0.0))

(defun default-d-loss (target output &rest other)
  (declare (ignore other))
  (- target output ))

;;尽量不要让每个权重都有相同的值
(defun rand ()
  (/ (- 5000 (random 10000)) 10000.0))

;;连接两个层之间的结构体
(defstruct connect-slab
  input
  weights
  bias
  sum
  output
  error)

(defclass network ()
  ((connect-slab-list
    :initform nil
    :accessor connect-slab-list)
   (connect-slab-relist
    :initform nil
    :accessor connect-slab-relist)
   (depth
    :initform 0
    :accessor depth
    :type (unsigned-byte 32))
   (learning-rate
    :initarg  :learning-rate
    :initform 0.5
    :accessor learning-rate
    :type single-float)
   (activation
    :initarg  :activation
    :initform #'sigmoid
    :accessor activation) ;;激活函数
   (d-activation
    :initarg  :d-activation
    :initform #'d-sigmoid
    :accessor d-activation) ;;激活函数的导数
   (d-loss
    :initarg  :d-loss
    :initform #'default-d-loss
    :accessor d-loss) ;;损失函数
   (other-d-loss-args
    :initarg  :other-d-loss-args
    :initform nil
    :accessor other-d-loss-args))) ;;添加额外计算参数

(defmacro let-if (sym expr &body body)
  `(let ((,sym ,expr))
     (if ,sym
	 ,@body)))

(defun make-double-array (size)
  (make-array size :element-type 'single-float))

(defmethod initialize-instance :after
    ((network network) &key size-list)
  (loop for sliding on size-list
	for in = (car sliding)
	for out = (cadr sliding)
	for depth = 1 then (1+ depth)
	with curr = nil
	with cslabs = nil
	while out do
	(progn
	  (setf curr
		(make-connect-slab
		 :weights	(make-double-array
				 (list in out))
		 :bias		(make-double-array out)
		 :sum		(make-double-array out)
		 :error		(make-double-array out)
		 :output	(make-double-array out)))
	  (setf (slot-value curr 'input)
		(let-if pre (car cslabs)
			(slot-value pre 'output)
			nil))
	  (dotimes (j out)
	    (dotimes (i in)
	      (setf (aref (slot-value curr 'weights) i j)
		    (rand)))
	    (setf (aref (slot-value curr 'bias) j)
		  (rand)))
	  (push curr cslabs))
	finally
	(progn
	  (setf (connect-slab-relist network) cslabs)
	  (setf (connect-slab-list network)
		(reverse cslabs))
	  (setf (depth network) depth))))

(defmethod forward-propagate ((network network))
  (with-accessors ((cslab-lst connect-slab-list)
		   (activation activation)) network
    (dolist (cslab cslab-lst)
      (with-slots (input weights bias sum output) cslab
	(dotimes (j (length output))
	  (setf (aref sum j) (aref bias j))
	  (dotimes (i (length input))
	    (incf (aref sum j)
		  (* (aref input i)
		     (aref weights i j))))
	  (setf (aref output j)
		(funcall activation (aref sum j))))))))

(defmethod backward-propagate ((network network) target)
  (with-accessors
	((cslab-lst connect-slab-relist)
	 (learning-rate learning-rate)
	 (d-activation d-activation)
	 (d-loss d-loss)
	 (other-d-loss-args other-d-loss-args)) network
    (loop for prev = nil then cslab
	  for cslab in cslab-lst do
      (with-slots (input weights bias sum output
			 error) cslab
	(dotimes (j (length output))
	  (setf (aref error j)
		(* (if prev
		       (let ((res 0))
			 (with-slots
			       (error weights) prev
			   ;;上一个连接处的输入就是
			   ;:当前连接的输出
			   (dotimes (i (length error))
			     (incf res
				   (* (aref error i)
				      (aref weights j i))))
			   res))
		       (funcall d-loss
				(aref target j)
				(aref output  j)
				other-d-loss-args))
		   (funcall d-activation
			    (aref sum     j))))
	  (dotimes (i (length input))
	    (incf (aref weights i j)
		  (* learning-rate
		     (aref error j)
		     (aref input i))))
	  (incf (aref bias j)
		(* learning-rate
		   (aref error j))))))))

(defmethod train ((network network) input target)
  (setf
   (slot-value (car (connect-slab-list network)) 'input)
   input)
  (forward-propagate network)
  (backward-propagate network target))

(defmethod run-training ((network network) epoch inputs targets)
  (dotimes (times epoch)
    (loop for inp in inputs
	  for tar in targets do
	  (progn
	    (train network inp tar)
	    (when (= (mod times 10000) 9999)
		(format t "~a~%~%" (connect-slab-list network)))))))

;;不一定能跑得出来，因为这个有些吃"运气"，如果初始值不好
;;容易掉进局部最优解里面
;;但是本人尝试了一下大多数情况下都能跑出来
;;不过没有尝试更加复杂的实际问题，需要优化
(let ((xor (make-instance 'network
			  :learning-rate 0.1
			  :size-list '(2 2 1))))
  (format t "~a~%" (connect-slab-list xor))
  (run-training xor 100000
		'(#(0 0) #(0 1) #(1 0) #(1 1))
		'(#(0)   #(1)   #(1)   #(0))))
