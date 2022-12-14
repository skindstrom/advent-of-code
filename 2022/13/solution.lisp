(defparameter *file* "2022/13/input.txt")

(defun parse-file ()
  (mapcar #'(lambda (pair)
	      (list (read-from-string (car pair))
		    (read-from-string (cadr pair))))
	  (cl-utilities:split-sequence ""
				       (mapcar (cl-utilities:compose
						(lambda (line) (substitute #\( #\[ line))
						(lambda (line) (substitute #\) #\] line))
						(lambda (line) (substitute #\space #\, line)))
					       (uiop:read-file-lines *file*))
				       :test 'string=)))
	    
(defun in-order-p (a b)
  (cond ((and (numberp a)
	      (numberp b))
	 (cond ((< a b) :less)
	       ((= a b) :equal)
	       (t :more)))
	((and (listp a)
	       (numberp b))
	 (in-order-p a (list b)))
	((and (numberp a)
	      (listp b))
	 (in-order-p (list a) b))
	((and (listp a)
	      (listp b))
	 (cond ((and (null a) (null b)) :equal)
	       ((null a) :less)
	       ((null b) :more)
	       (t (let ((result (in-order-p (car a) (car b))))
		    (if (eq result :equal)
			(in-order-p (cdr a) (cdr b))
			result)))))))

(defun solution-1 ()
  (loop for pair in (parse-file)
	and idx from 1
	sum (let ((result (in-order-p (car pair) (cadr pair))))
		  (if (or (eq result :equal)
			  (eq result :less))
		      idx
		      0))))

(defun solution-2 ()
  (let* ((packets (reduce #'(lambda (acc pair)
				(cons (car pair)
				      (cons (cadr pair)
					    acc)))
			    (parse-file)
			    :initial-value '(((2)) ((6)))))
	 (sorted (sort packets #'(lambda (a b) (eq (in-order-p a b) :less)))))
    (*
     (1+ (position '((2)) sorted :test 'equal))
     (1+ (position '((6)) sorted :test 'equal)))))