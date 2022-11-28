(defun solution-1 ()
  (1+ (let ((input (read-lines)))
           (loop for prev in input
                 and cur in (cdr input)
                 sum (if (> cur prev) 1 0)))))

(defun solution-2 ()
  (let ((input (get-threes)))
    (loop for prev in input
	  and cur in (cdr input)
	  sum (if (> cur prev) 1 0))))

(defun get-threes ()
  (let ((input (read-lines)))
    (loop for first in input
          and second in (cdr input)
          and third in (cdr (cdr input))
          collect (+ first second third))))

(defun read-lines ()
  (mapcar #'parse-integer (uiop:read-file-lines "input.txt")))
