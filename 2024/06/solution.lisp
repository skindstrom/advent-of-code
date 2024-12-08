(use-package :alexandria)
(defparameter *file* (pathname "input.txt"))

(defun solution-1 ()
  (let* ((arr (parse-file))
         (all-updates (tick-until-done arr (find-starting-position arr))))
    (length (remove-duplicates all-updates
                               :test #'equal
                               :key (lambda (update)
                                      ; don't care about the direction we went
                                      (list (first update)
                                            (second update)))))))

(defun solution-2 ()
  "
Count the amount of possible places to put an obstacle to cause an infinite loop
We know it's an infinite loop if they walk on the same place twice, in the same direction
So let's make sure to catch that in the update
For every tick, turn the guard and loop through until they either leave the area, or until they loop forever
"
  "TODO")

(defun tick-until-done (arr pos)
  "Returns all the updates performed, or nil if infinite loop"
  (labels ((iter (all)
             (let ((update (tick arr
                                 (caar all)
                                 (cadar all))))
               (cond ((null update) all)
                     ; infinite loop (getting slow, consider hash table)
                     ((find update all :test #'equal) nil)
                     (t (iter (cons update all)))))))
    (iter (list (list (first pos)
                (second pos)
                (aref arr (first pos) (second pos)))))))

(defun tick (arr y x)
  "Returns the new position, or nil if outside of the map"
  (let ((dir (aref arr y x)))
    (destructuring-bind (new-y new-x)
        (switch (dir :test #'char=)
          (#\^ (list (1- y) x))
          (#\> (list y (1+ x)))
          (#\v (list (1+ y) x))
          (#\< (list y (1- x))))
      (when (and (< -1 new-y (array-dimension arr 0))
                 (< -1 new-x (array-dimension arr 1)))
        (if (char= #\# (aref arr new-y new-x))
            (progn (setf (aref arr y x)
                         (turn (aref arr y x)))
                   (tick arr y x))
            (progn
              (rotatef (aref arr y x)
                       (aref arr new-y new-x))
             (list new-y new-x dir)))))))

(defun turn (char)
  (switch (char :test #'char=)
    (#\^ #\>)
    (#\> #\v)
    (#\v #\<)
    (#\< #\^)))

(defun find-starting-position (arr)
  (dotimes (i (array-dimension arr 0))
    (dotimes (j (array-dimension arr 1))
      (when (some (lambda (c) (char= (aref arr i j)
                                    c))
                 '(#\^ #\> #\v #\<))
        (return-from find-starting-position (list i j))))))

(defun parse-file ()
  (let ((lines (uiop:read-file-lines *file*)))
    (make-array (list (length lines)
                      (length (car lines)))
                :initial-contents lines)))
