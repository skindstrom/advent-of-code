(defun or-f (a b)
  (or a b))

(defun and-f (a b)
  (and a b))

(defun bingo? (board marked)
  (or (aops:reduce-index #'or-f i
	(aops:reduce-index #'and-f j
	  (gethash (aref board i j) marked)))
      (aops:reduce-index #'or-f i
	(aops:reduce-index #'and-f j
	  (gethash (aref board j i) marked)))))

(defun parse-moves (line)
  (mapcar #'parse-integer (cl-utilities:split-sequence #\, line)))

(defun parse-boards (lines)
  (let ((tmp (cl-utilities:split-sequence nil
			       (mapcar (cl-utilities:compose
					(lambda (row) (mapcar (lambda (val) (parse-integer val :junk-allowed t)) row))
					(lambda (row) (remove-if (lambda (val) (string= val "")) row))
					(lambda (line) (cl-utilities:split-sequence #\space line)))
				       lines))))
    (make-array (list (length tmp) 5 5) :element-type 'number :initial-contents tmp)))

(defun parse-file ()
  (let ((lines (uiop:read-file-lines "2021/04/input.txt")))
    (cons (parse-moves (car lines))
	  (parse-boards (cddr lines)))))

(defun sum-unmarked (board marked)
  (reduce (lambda (acc val) (if (not (gethash val marked))
					   (+ acc val)
					   acc))
	  (aops:flatten board)
	  :initial-value 0))

(defun solution-1 ()
  (let* ((input (parse-file))
	 (draws (car input))
	 (boards (cdr input)))
    (loop for draw in draws
	  and idx from 0
	  with marked = (make-hash-table)
	  until (find-if (lambda (board) (bingo? board marked))
			 (aops:split boards 1))
	  do (setf (gethash draw marked) marked)
	  finally (return (* (elt draws (1- idx))
			     (sum-unmarked
			      (find-if
			       (lambda (board)
				 (bingo? board marked))
			       (aops:split boards 1))
			      marked))))))

(defun solution-2 ()
  (let* ((input (parse-file))
	 (draws (car input))
	 (boards (cdr input))
	 (marked (let ((tbl (make-hash-table)))
		   (loop for draw in draws
			 do (setf (gethash draw tbl) tbl))
		   tbl)))
    (loop for draw in (reverse draws)
	  with boards = (aops:split boards 1)
	  do (remhash draw marked)
	     (let ((board (find-if (lambda (board) (not (bingo? board marked))) boards)))
	       (if board
		   (progn
		     (setf (gethash draw marked) marked)
		     (return (* draw (sum-unmarked board marked)))))))))
