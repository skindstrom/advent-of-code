(defparameter *file* "2022/08/input.txt")

(defun chunk (sequence size)
  (loop for i below (length sequence) by size
	collect (subseq sequence i (1+ (* i size)))))

(defun parse-file ()
  (let ((input (mapcar (cl-utilities:compose
	     (lambda (nums) (mapcar #'parse-integer nums))
	     (lambda (row) (chunk row 1)))
		       (uiop:read-file-lines *file*))))
    (make-array (list (length input) (length (car input)))
		:initial-contents input)))

(defun position-in-direction (predicate map i j direction)
  (destructuring-bind (n m) (array-dimensions map)
    (let ((step (lambda () (ecase direction
			      (up (setf i (1- i)))
			      (down (setf i (1+ i)))
			      (right (setf j (1+ j)))
			      (left (setf j (1- j)))))))
      (funcall step)
      (loop while (and (<= 0 i (1- n))
		       (<= 0 j (1- m)))
	    do (if (funcall predicate (aref map i j))
		   (return (list i j)))
	       (funcall step)))))

(defun is-visible (map i j)
  (let* ((height (aref map i j))
	 (predicate (lambda (other-height) (<= height other-height))))
    (not (and (position-in-direction predicate map i j 'up)
	  (position-in-direction predicate map i j 'right)
	  (position-in-direction predicate map i j 'down)
	  (position-in-direction predicate map i j 'left)))))
	       

(defun solution-1 ()
  (let ((map (parse-file)))
    (destructuring-bind (n m) (array-dimensions map)
      (loop for i below n
	    sum (loop for j below m
		      count (is-visible map i j))))))
    
(defun scenic-score (map i j)
  (destructuring-bind (n m) (array-dimensions map)
    (let* ((height (aref map i j))
	 (predicate (lambda (other-height) (<= height other-height))))
      (* (let ((x (car (position-in-direction predicate map i j 'up))))
	      (if x
		  (abs (- i x))
		  i))
	    (let ((y (cadr (position-in-direction predicate map i j 'left))))
	      (if y
		  (abs (- j y))
		  j))
	    (let ((x (car (position-in-direction predicate map i j 'down))))
	      (if x
		  (- x i)
		  (1- (- n i))))
	    (let ((y (cadr (position-in-direction predicate map i j 'right))))
	      (if y
		  (- y j)
		  (1- (- m j))))))))

(defun solution-2 ()
    (let ((map (parse-file)))
    (destructuring-bind (n m) (array-dimensions map)
      (loop for i below n
	    maximize (loop for j below m
		      maximize (scenic-score map i j))))))
