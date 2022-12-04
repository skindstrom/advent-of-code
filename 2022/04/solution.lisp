(defun parse-range (range)
  (let ((range (mapcar #'parse-integer (cl-utilities:split-sequence #\- range))))
    (cons (car range) (cadr range))))

(defun parse-file ()
  (map 'vector (lambda (line) (map 'vector #'parse-range (cl-utilities:split-sequence #\, line)))
       (uiop:read-file-lines "2022/04/input.txt")))

(defun ranges-contain (pair)
  (let ((range-1 (elt pair 0))
	(range-2 (elt pair 1)))
    (or (<= (car range-1) (car range-2) (cdr range-2) (cdr range-1))
	(<= (car range-2) (car range-1) (cdr range-1) (cdr range-2)))))

(defun are-overlapping (pair)
  (let ((range-1 (elt pair 0))
	(range-2 (elt pair 1)))
    (or (<= (car range-2) (cdr range-1) (cdr range-2))
	(<= (car range-2) (cdr range-1) (cdr range-2))
	(<= (car range-1) (car range-2) (cdr range-1))
	(<= (car range-1) (cdr range-2) (cdr range-1)))))

(defun are-overlapping-at-all (pair)
  (or (ranges-contain pair)
      (are-overlapping pair)))

(defun solution-1 ()
  (count-if #'ranges-contain (parse-file)))

(defun solution-2 ()
  (count-if #'are-overlapping-at-all (parse-file)))
