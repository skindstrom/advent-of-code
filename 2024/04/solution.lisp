(defparameter *file* (pathname "input.txt"))

(defun solution-1 ()
  (xmas-finder (parse-file)))

(defun xmas-finder (arr)
  "Go through each index in the array. For every X, search in each direction. If XMAS can be completed, count that index once"
  (let ((sum 0))
    (dotimes (row (array-dimension arr 0))
      (dotimes (col (array-dimension arr 1))
        (incf sum (xmas-count arr row col))))
    sum))

(defun xmas-count (arr row col)
  "Returns the amount of times that xmas starts from the current index"
  (labels ((iter (expected direction row col)
             (if (eq expected :done)
                 t
                 (when (and (< -1 row (array-dimension arr 0))
                            (< -1 col (array-dimension arr 1))
                            (char= expected (aref arr row col)))
                   (case direction
                     (:up (iter (next-letter expected) :up (1- row) col))
                     (:down (iter (next-letter expected) :down (1+ row) col))
                     (:left (iter (next-letter expected) :left row (1- col)))
                     (:right (iter (next-letter expected) :right row (1+ col)))
                     (:up-left (iter (next-letter expected) :up-left (1- row) (1- col)))
                     (:up-right (iter (next-letter expected) :up-right (1- row) (1+ col)))
                     (:down-left (iter (next-letter expected) :down-left (1+ row) (1- col)))
                     (:down-right (iter (next-letter expected) :down-right (1+ row) (1+ col))))))
             ))
    (reduce (lambda (sum direction)
            (if (iter #\X direction row col)
                (1+ sum)
                sum))
          '(:up :down
            :left :right
            :up-left :up-right
            :down-left :down-right)
          :initial-value 0)))

(defun next-letter (curr)
  "Returns :done when all are achieved, and nil if invalid"
  (cond ((char= curr #\X) #\M)
        ((char= curr #\M) #\A)
        ((char= curr #\A) #\S)
        ((char= curr #\S) :done)))

(defun parse-file ()
  "Returns a NxM array of characters"
  (let ((lines (uiop:read-file-lines *file*)))
    (make-array (list (length lines)
                      (length (first lines)))
                :initial-contents lines)))
