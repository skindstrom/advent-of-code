(defun solution-1 ()
  (reduce #'+
	  (mapcar #'(lambda (round)
		      (+ (score-outcome round)
			 (score-shape (cadr round))))
		  (parse-file))))

(defun solution-2 ()
  (reduce #'+
	  (mapcar #'(lambda (round)
		      (+ (score-outcome round)
			 (score-shape (cadr round))))
		  (mapcar #'update-move-for-round
			  (parse-file)))))

(defun update-move-for-round (round)
  (loop for move in '(x y z)
	with wanted-score = (ecase (cadr round)
			      (x 0)
			      (y 3)
			      (z 6))
	do (if (= wanted-score (score-outcome (list (car round) move)))
	       (return (list (car round) move)))))

			     

(defun score-shape (shape)
  (ecase shape
    (x 1)
    (y 2)
    (z 3)))


(defun score-outcome (round)
  (cond ((equal round '(a x)) 3)
	((equal round '(a y)) 6)
	((equal round '(a z)) 0)
	((equal round '(b x)) 0)
	((equal round '(b y)) 3)
	((equal round '(b z)) 6)
	((equal round '(c x)) 6)
	((equal round '(c y)) 0)
	((equal round '(c z)) 3)
	(t "invalid")))
	

(defun parse-file ()
  (mapcar #'parse-line (uiop:read-file-lines "2022/02/input.txt")))


(defun parse-line (line)
  (mapcar #'parse-shape (uiop:split-string line)))

(defun parse-shape (shape)
  (cond ((string= shape "A") 'a)
	((string= shape "B") 'b)
	((string= shape "C") 'c)
	((string= shape "X") 'x)
	((string= shape "Y") 'y)
	((string= shape "Z") 'z)))
