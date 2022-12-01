(defun solution-1 ()
  (reduce #'max (mapcar #'sum (group-by (read-file)))))

(defun solution-2 ()
  (sum (reduce #'top-3
	       (mapcar #'sum
		   (group-by (read-file)))
	   :initial-value (list -1 -1 -1))))

(defun top-3 (list-of-three-elements x)
  (cdr (sort (cons x list-of-three-elements) #'<)))

(defun group-by (input)
  (reduce #'(lambda (acc x)
	      (if x 
		  (cons (cons x (car acc)) (cdr acc))
		  (cons (list) acc)))
	  input
	  :initial-value (list)))

(defun sum (list)
  (reduce #'+ list))

(defun read-file ()
  (mapcar #'(lambda (line) (parse-integer line :junk-allowed t)) (uiop:read-file-lines "2022/01/input.txt")))
