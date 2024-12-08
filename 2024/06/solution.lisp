(use-package :alexandria)
(defparameter *file* (pathname "input.txt"))

(defun solution-1 ()
  (let* ((arr (parse-file))
         (all-pos (tick-until-done arr (find-starting-position arr))))
    (length (remove-duplicates all-pos :test #'equal))))

(defun tick-until-done (arr pos)
  "Returns all the updates performed"
  (labels ((iter (all)
             (let ((update (tick arr
                                 (caar all)
                                 (cadar all))))
               (if (null update)
                   all
                   (iter (cons update all))))))
    (iter (list pos))))

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
             (list new-y new-x)))))))

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
