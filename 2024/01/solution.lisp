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

(defun solution-2 ()
  (destructuring-bind (first second) (merge-pairs (read-file))
    (let ((occurences (occurence-map second)))
      (reduce #'(lambda (sum val)
                  (+ sum
                     (* val
                        (gethash val occurences 0))))
              first
              :initial-value 0))))

(defun occurence-map (l)
  (labels ((iter (l ht)
             (if l
                 (progn
                   (setf (gethash (car l) ht)
                         (1+ (or (gethash (car l) ht)
                                 0)))
                   (iter (cdr l)
                         ht))
                 ht)))
    (iter l (make-hash-table))))

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
