(defparameter *file* (pathname "input.txt"))

(defun solution-1 ()
  (reduce #'+
          (mapcar #'(lambda (pair) (abs (- (first pair)
                                   (second pair))))
          (apply #'mapcar
                 #'list
                 (mapcar #'(lambda (col)
                             (sort col #'<))
                         (merge-pairs (read-file)))))
          :initial-value 0))

(defun merge-pairs (pairs)
  (labels ((iter (pairs x y)
                (if pairs
                    (iter (cdr pairs)
                          (cons (caar pairs) x)
                          (cons (cadar pairs) y))
                    (list x y))))
    (iter pairs nil nil)))

(defun read-file ()
  "Returns a list of pairs"
  (mapcar #'(lambda (line)
              (mapcar #'parse-integer
                      (remove "" (uiop:split-string line) :test #'string=)))
          (uiop:read-file-lines *file*)))
