(defparameter *file* "2022/12/input.txt")


(defun parse-file ()
  (let* ((lines (uiop:read-file-lines *file*))
	 (n (length lines))
	 (m (length (car lines))))
    (make-array (list n m) :initial-contents lines)))

(defun can-move (map pos-from pos-to)
  (if (and (<= 0 (car pos-to) (1- (array-dimension map 0)))
	  (<= 0 (cdr pos-to) (1- (array-dimension map 1))))
      (let ((from (aref map (car pos-from) (cdr pos-from)))
	    (to (aref map (car pos-to) (cdr pos-to))))
	(cond ((char= from #\E) nil)
	      ((char= to #\E) (char= from #\z))
	      ((char= from #\S) t)
	      ((char= to #\S) nil)
	      (t (>= 1 (- (char-code to)
			  (char-code from))))))))

(defun possible-moves (map from-pos)
  (remove-if-not #'(lambda (to-pos) (can-move map from-pos to-pos))
		 (list (cons (1- (car from-pos)) (cdr from-pos))
		       (cons (1+ (car from-pos)) (cdr from-pos))
		       (cons (car from-pos) (1- (cdr from-pos)))
		       (cons (car from-pos) (1+ (cdr from-pos))))))

(defun find-elevation (map elevation)
  (loop for i below (array-total-size map)
	do (if (char= (row-major-aref map i) elevation)
	       (let ((column-count (array-dimension map 1)))
		 (return (cons (floor (/ i column-count))
			       (mod i column-count)))))))

(defun find-elevations (map elevation)
  (remove-if #'null
	     (loop for i below (array-total-size map)
		   collect (if (char= (row-major-aref map i) elevation)
			       (let ((column-count (array-dimension map 1)))
				 (cons (floor (/ i column-count))
				       (mod i column-count)))))))

(defun path-to-target (map pos-start)
  (let ((visited (make-hash-table :test 'equal)))
    (defun inner (path pos)
      (if (>= (length path) (gethash pos visited 10000000))
	  nil
	  (progn
	    (setf (gethash pos visited) (length path))
	    (if (char= (aref map (car pos) (cdr pos)) #\E)
		path
		(reduce (lambda (shortest-path to-pos)
			  (let ((new-path (inner (cons pos path) to-pos)))
			      (if (or (not shortest-path)
				      (and new-path
					   (< (length new-path)
					      (length shortest-path))))
				  new-path
				  shortest-path)))
			(possible-moves map pos)
			:initial-value nil)))))
    (nreverse (inner nil pos-start))))

(defun solution-1 ()
  (let ((map (parse-file)))
    (length (path-to-target map (find-elevation map #\S)))))

(defun solution-2 ()
  (let ((map (parse-file))
	(cnt 0))
    (reduce #'(lambda (shortest-path pos-start)
		(format t "~a ~%" cnt)
		(setf cnt (1+ cnt))
		(let ((new-path (path-to-target map pos-start)))
		  (if new-path
		      (min shortest-path (length (path-to-target map pos-start)))
		      shortest-path)))
	    (find-elevations map #\a)
	    :initial-value 100000000)))
