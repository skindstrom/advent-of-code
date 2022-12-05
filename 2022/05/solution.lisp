(defparameter *file* "2022/05/input.txt")

(defun parse-stacks (list-of-str)
  (let* ((list-of-str (nreverse list-of-str))
	 (stack-count (apply #'max (mapcar (lambda (c) (or (parse-integer c :junk-allowed t) 0))
					   (cl-utilities:split-sequence #\space (car list-of-str)))))
	 (input-stacks (mapcar (lambda (line)
				 (mapcar (lambda (match) (if (char= #\[ (elt match 0))
							     (elt match 1)))
					 (ppcre:all-matches-as-strings "( {4}| {3}|\\[[A-Z]\\] ?)" line)))
				 (cdr list-of-str)))
	 (stacks (make-array stack-count :initial-element nil)))
    (loop for row in input-stacks
	  do (loop for container in row
		   and stack-idx from 0
		   do (if container
			  (setf (aref stacks stack-idx) (cons container (aref stacks stack-idx))))))
    stacks))

(defun parse-moves (list-of-str)
  (mapcar (lambda (line) (map 'vector
			      'parse-integer
			      (ppcre:all-matches-as-strings "\\d+" line)))
	    list-of-str))

(defun parse-file ()
  (let* ((input (cl-utilities:split-sequence "" (uiop:read-file-lines *file*) :test 'string=))
	 (stacks (parse-stacks (car input)))
	 (moves (parse-moves (cadr input))))
    (list stacks moves)))


(defun perform-move (stacks move)
  (let ((cnt (elt move 0))
	(from (1- (elt move 1)))
	(to (1- (elt move 2))))
  (loop for i below cnt
	do (setf (aref stacks to) (cons (car (aref stacks from))
					(aref stacks to)))
	   (setf (aref stacks from) (cdr (aref stacks from))))))

(defun perform-move-cratemover-9001 (stacks move)
  (let ((cnt (elt move 0))
	(from (1- (elt move 1)))
	(to (1- (elt move 2))))
    (setf (aref stacks to) (append (subseq (aref stacks from) 0 cnt)
				   (aref stacks to)))
    (setf (aref stacks from) (subseq (aref stacks from) cnt))))
  

(defun solution-1 ()
  (destructuring-bind (stacks moves) (parse-file)
    (loop for move in moves
	  do (perform-move stacks move))
    (loop for stack across stacks
	  when stack
	    do (format t "~a" (car stack)))))

(defun solution-2 ()
  (destructuring-bind (stacks moves) (parse-file)
    (loop for move in moves
	  do (perform-move-cratemover-9001 stacks move))
    (loop for stack across stacks
	  when stack
	  do (format t "~a" (car stack)))))
