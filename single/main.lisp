(defpackage :gate
  (:use :common-lisp))

(in-package :gate)

(defun rand ()
  (/ (random 100) 100.0))

(defmacro make-double-array (nums)
  `(make-array ,nums :element-type 'single-double))

(defstruct delta2
  input
  weights
  sum
  bias
  output)

(defun sigmoid (x)
  (/ 1.0 (+ 1.0 (exp (- x)))))

(defun d-sigmoid (y)
  (* y (- 1 y)))

(defun create-delta2 (input-num output-num)
  (let ((delta2 (make-delta2
				  :input nil
				  :weights (make-double-array (list input-num output-num))
				  :sum (make-double-array output-num)
				  :bias (make-double-array output-num)
				  :output (make-double-array output-num))))
	(with-slots (bias weights) delta2
	  (loop for j from 0 below output-num do
			(progn
			  (setf (aref bias j) (rand))
			  (loop for i from 0 below input-num do
					(setf (aref weights i j) (rand))))))
	delta2))

(defun forward-pass (delta2)
  (with-slots (input weights sum bias output) delta2
	(dotimes (j (length sum))
	  (setf (aref sum j) (aref bias j)))
	(dotimes (i (length input))
	  (dotimes (j (length output))
		(incf (aref sum j) (* (aref input i) 
							  (aref weights i j)))))
	(dotimes (j (length output))
	  (setf (aref output j) (sigmoid (aref sum j))))))

(defun train (delta2 inp target learning-rate)
  (with-slots (input weights sum bias output) delta2
	(setf input inp)
	(forward-pass delta2)
	(dotimes (j (length output))
	  (dotimes (i (length input))
		(incf (aref weights i j) 
			  (* (- (aref target j) (aref output j))
				 (d-sigmoid (aref output j)) (aref input i)
				 learning-rate)))
	  (incf (aref bias j)
			(* (- (aref target j) (aref output j))
			   (d-sigmoid (aref output j)) learning-rate)))))

(defun run-training (delta2 inputs targets epoch learning-rate)
  (dotimes (times epoch)
	(loop for input in inputs
		  for target in targets do
		  (progn
			(train delta2 input target learning-rate)
			(when (= (mod times 50) 0)
			  (format t "bias: ~a~%"
					  (slot-value delta2 'bias))
			  (format t "target: ~a~%" target)
			  (format t "output: ~a~%" 
					  (slot-value delta2 'output))
			  (format t "input: ~a~%"
					  (slot-value delta2 'input))
			  (format t "weights: ~a~%~%"
					  (slot-value delta2 'weights)))))))

(let ((and-gate (create-delta2 2 1))
	  (or-gate (create-delta2 2 1)))
  (format t "train and gate~%")
  (run-training and-gate
				'(#(0 0) #(0 1) #(1 0) #(1 1))
				'(#(0)   #(0)   #(0)   #(1))
				300 0.5)
  (format t "train or gate~%")
  (run-training or-gate
				'(#(0 0) #(0 1) #(1 0) #(1 1))
				'(#(0)   #(1)   #(1)   #(1))
				300 0.5))
