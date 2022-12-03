(defun parse-line (line)
  (map 'vector #'digit-char-p line))

(defun parse-file ()
  (let ((vecs (map 'vector #'parse-line (uiop:read-file-lines "2021/03/input.txt"))))
    (make-array (list (length vecs) (length (aref vecs 0))) :initial-contents vecs)))

(defun transpose (arr)
  (aops:each-index (i j) (aref arr j i)))

(defun binary->decimal (binary-list)
  (reduce (lambda (x y) (+ (* 2 x) y)) binary-list))

(defun gamma-rate (arr)
  (binary->decimal (bit-criteria arr #'> 1 0)))

(defun epsilon-rate (arr)
  (binary->decimal (bit-criteria arr #'> 0 1)))

(defun bit-criteria (arr criteria match non-match)
  (aops:each-index i
    (if (funcall criteria
		 (aops:reduce-index #'+ j (aref arr j i))
		 (/ (aops:dim arr 0 ) 2))
	match
	non-match)))


(defun solution-1 ()
  (let ((arr (parse-file)))
    (* (gamma-rate arr) (epsilon-rate arr))))

(defun oxygen-generator-rating (arr)
  (destructuring-bind (row-count column-count) (array-dimensions arr)
    (let ((row (loop for column below column-count
		     with eligible-rows = (loop for row below row-count collect row)
		     when (> (length eligible-rows) 1)
		       do (setf eligible-rows 
				(loop for row in eligible-rows
				      with criteria = (if (>= (loop for row in eligible-rows
								    sum (aref arr row column))
							      (/ (length eligible-rows) 2))
							  1
							  0)
				      when (= (aref arr row column) criteria)
					collect row))
		     finally (return (car eligible-rows)))))
      (binary->decimal (loop for column below column-count
			     collect (aref arr row column))))))

(defun co2-scrubber-rating (arr)
  (destructuring-bind (row-count column-count) (array-dimensions arr)
    (let ((row (loop for column below column-count
		     with eligible-rows = (loop for row below row-count collect row)
		     when (> (length eligible-rows) 1)
		       do (setf eligible-rows 
				(loop for row in eligible-rows
				      with criteria = (if (>= (loop for row in eligible-rows
								    sum (aref arr row column))
							      (/ (length eligible-rows) 2))
							  0
							  1)
				      when (= (aref arr row column) criteria)
					collect row))
		     finally (return (car eligible-rows)))))
      (binary->decimal (loop for column below column-count
			     collect (aref arr row column))))))

(defun solution-2 ()
  (let ((arr (parse-file)))
    (* (oxygen-generator-rating arr) (co2-scrubber-rating arr))))

(defun example-input ()
  (make-array '(12 5)
	      :initial-contents '((0 0 1 0 0)
				 (1 1 1 1 0)
				 (1 0 1 1 0)
				 (1 0 1 1 1)
				 (1 0 1 0 1)
				 (0 1 1 1 1)
				 (0 0 1 1 1 )
				 (1 1 1 0 0 )
				 (1 0 0 0 0)
				 (1 1 0 0 1)
				 (0 0 0 1 0)
				 (0 1 0 1 0))))
	      
