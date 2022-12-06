(defparameter *file* "2022/06/input.txt")
(defparameter *marker-length* 4)
(defparameter *message-length* 14)

(defun parse-file ()
  (uiop:read-file-line *file*))

(defun has-duplicate-char (substr)
  (loop for i below (1- (length substr))
	do (if (loop for j from (1+ i) below (length substr)
		     do (if (char= (char substr i)
				   (char substr j))
			    (return t)))
	       (return t))))

(defun is-marker (substr)
  (not (has-duplicate-char substr)))

(defun solution-1 ()
  (let ((cipher (parse-file)))
    (loop for i from *marker-length* to (length cipher)
	  do (if (is-marker (subseq cipher (- i *marker-length*) i))
		 (return i)))))

(defun solution-2 ()
  (let ((cipher (parse-file)))
    (loop for i from *message-length* to (length cipher)
	  do (if (is-marker (subseq cipher (- i *message-length*) i))
		 (return i)))))



