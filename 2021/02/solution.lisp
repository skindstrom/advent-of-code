(defun solution-1 ()
  (loop for entry in (read-file)
	with horizontal = 0
	with depth = 0
	do (let ((action (car entry))
		 (value (cdr entry)))
	     (cond ((string= action "forward") (setf horizontal (+ horizontal value)))
		   ((string= action "up") (setf depth (- depth value)))
		   ((string= action "down") (setf depth (+ depth value)))
		   (t (format t "no matches"))))
	finally (return (* horizontal depth))))

(defun solution-2 ()
  (loop for entry in (read-file)
	with horizontal = 0
	with depth = 0
	with aim = 0
	do (let ((action (car entry))
		 (value (cdr entry)))
	     (cond ((string= action "forward") (progn
						 (setf horizontal (+ horizontal value))
						 (setf depth (+ depth (* aim value)))))
		   ((string= action "up") (setf aim (- aim value)))
		   ((string= action "down") (setf aim (+ aim value)))
		   (t (format t "no matches"))))
	finally (return (* horizontal depth))))


(defun read-file ()
  (mapcar #'(lambda (line) (let ((splits (uiop:split-string line)))
			     (cons (elt splits 0)
				   (parse-integer (elt splits 1)))))
	      (uiop:read-file-lines "2021/02/input.txt")))
