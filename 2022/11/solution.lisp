(defparameter *file* "2022/11/example.txt")

(defstruct monkey
  id
  items
  operation
  test
  true-monkey
  false-monkey)

(defun parse-items (string)
  (mapcar #'parse-integer
		    (cl-utilities:split-sequence #\,
						 (subseq string 18))))

(defun parse-operation (string)
  (lambda (old)
    (funcall (if (char= (char string 23) #\+)
		 #'+
		 #'*)
	     old
	     (let ((val (parse-integer (subseq string 25) :junk-allowed t)))
	       (if val
		   val
		   old)))))

(defun parse-test (string)
  (lambda (number)
    (= 0
       (mod number
	    (parse-integer (subseq string 21))))))

(defun parse-monkey-destination (string)
  (parse-integer (subseq (reverse string) 0 1)))

(defun parse-monkey (id lines)
  (make-monkey
   :id id
   :items (parse-items (nth 1 lines))
   :operation (parse-operation (nth 2 lines))
   :test (parse-test (nth 3 lines))
   :true-monkey (parse-monkey-destination (nth 4 lines))
   :false-monkey (parse-monkey-destination (nth 5 lines))))

(defun parse-file ()
  (let ((in (loop for monkey-sequence in 
			    (cl-utilities:split-sequence-if #'(lambda (line) (= (length line)
										0))
							    (uiop:read-file-lines *file*))
	and monkey-idx from 0
		  collect (parse-monkey monkey-idx monkey-sequence))))
    (make-array (length in) :initial-contents in)))

(defun worry-level (monkey item)
  (funcall (monkey-operation monkey) item))

(defun monkey-destination (monkey item)
  (if (funcall (monkey-test monkey)
	       (worry-level monkey item))
      (monkey-true-monkey monkey)
      (monkey-false-monkey monkey)))

(defun monkey-toss-opt (monkeys from-id)
  (let ((item (pop (monkey-items (aref monkeys from-id))) ))
    (setf (monkey-items (aref monkeys
			      (monkey-destination (aref monkeys from-id)
						  item)))
	  (append (monkey-items (aref monkeys
			      (monkey-destination (aref monkeys from-id)
						  item)))
		  (list (worry-level (aref monkeys from-id) item))))))

(defun monkey-turn-opt (monkeys from-id callback)
  (loop while (monkey-items (aref monkeys from-id))
	do (monkey-toss-opt monkeys from-id)
	   (funcall callback from-id)))

(defun monkey-round-opt (monkeys callback)
  (loop for id below (length monkeys)
	do (monkey-turn-opt monkeys id callback)))

(defun monkey-toss (monkeys from-id)
  (let* ((item (car (monkey-items (nth from-id monkeys))))
	 (to-id (monkey-destination (nth from-id monkeys) item)))
    (mapcar #'(lambda (monkey)
		(cond ((= from-id (monkey-id monkey))
		       (make-monkey :id (monkey-id monkey)
				    :items (cdr (monkey-items monkey))
				    :operation (monkey-operation monkey)
				    :test (monkey-test monkey)
				    :true-monkey (monkey-true-monkey monkey)
				    :false-monkey (monkey-false-monkey monkey)))
		      ((= to-id (monkey-id monkey))
		       (make-monkey :id (monkey-id monkey)
				    :items (append (monkey-items monkey) (list (worry-level (nth from-id monkeys) item)))
				    :operation (monkey-operation monkey)
				    :test (monkey-test monkey)
				    :true-monkey (monkey-true-monkey monkey)
				    :false-monkey (monkey-false-monkey monkey)))
		      (t monkey)))
	    monkeys)))

(defun monkey-turn (monkeys id toss-callback)
  (if (not (monkey-items (nth id monkeys)))
      monkeys
      (let ((monkeys (monkey-toss monkeys id)))
	(funcall toss-callback id)
	(monkey-turn monkeys id toss-callback))))

(defun monkey-round (monkeys toss-callback)
  (defun inner (monkeys id)
    (if (< id (length monkeys))
	(inner (monkey-turn monkeys id toss-callback) (1+ id))
	monkeys))
  (inner monkeys 0))

(defun monkey-business (arr)
  (let ((max-val (reduce #'max arr)))
    (* max-val
       (reduce #'max (remove max-val arr)))))
		      

(defun solution-1 ()
  (monkey-business (let ((throw-counts (make-array 10 :initial-element 0)))
		     (loop for i below 20
			   with monkeys = (parse-file)
			   do (setf monkeys
				    (monkey-round monkeys
						  #'(lambda (id)
						      (setf (aref throw-counts id)
							    (1+ (aref throw-counts id)))))))
		     throw-counts)))

(defun solution-2 ()
  (let ((throw-counts (make-array 10 :initial-element 0)))
    (loop for i below 1000
	  with monkeys = (parse-file)
	  do (monkey-round-opt monkeys
			       #'(lambda (id)
				   (setf (aref throw-counts id)
					 (1+ (aref throw-counts id))))))
    throw-counts))
