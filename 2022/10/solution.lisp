(defparameter *file* "2022/10/input.txt")

(defun parse-file ()
  (mapcar #'(lambda (line)
	      (let ((instruction (read-from-string (subseq line 0 4))))
		(list instruction
		      (case instruction
			(addx (parse-integer (subseq line 5)))
			(noop nil)))))
	  (uiop:read-file-lines *file*)))

(defun run (instructions callback)
  (loop for instruction in instructions
	with cycle = 1
	with reg-x = 1
	do (funcall callback reg-x cycle)
	   (ecase (car instruction)
	     (noop (setf cycle (1+ cycle)))
	     (addx (progn (setf cycle (1+ cycle))
			  (funcall callback reg-x cycle)
			  (setf cycle (1+ cycle))
			  (setf reg-x (+ reg-x (cadr instruction)))))))))
			  
(defun solution-1 ()
  (let ((signal-strength 0))
    (run (parse-file)
	 #'(lambda (reg-x cycle)
	     (if (= 0 (mod (- cycle 20) 40))
		 (setf signal-strength (+ (* reg-x
						cycle)
					     signal-strength)))))
    signal-strength))

(defun print-crt (crt)
  (destructuring-bind (n m) (array-dimensions crt)
    (loop for i below n
	  do (loop for j below m
		   do (format t "~a" (aref crt i j)))
	     (format t "~%"))))

(defun solution-2 ()
  (print-crt (let ((crt (make-array '(6 40) :initial-element #\.)))
	       (run (parse-file)
		    #'(lambda (reg-x cycle)
			(if (<= (abs (- (mod (1- cycle) 40) reg-x)) 1)
			    (setf (row-major-aref crt (1- cycle)) #\#))))
	       crt)))
