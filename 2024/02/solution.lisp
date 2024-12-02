(defparameter *file* (pathname "input.txt"))
(defun solution-1 ()
  (count-if #'is-safe (read-reports)))

(defun is-safe (report)
  (and (is-increasing-or-decreasing report)
       (is-adjacent-levels-safe report)))

(defun is-increasing-or-decreasing (report)
  (or (reduce (lambda (max val)
                (when (and max
                           (> max val))
                  val)) report)
      (reduce (lambda (min val)
                (when (and min
                           (< min val))
                  val))
              report)))

(defun is-adjacent-levels-safe (report)
  (every (lambda (val) (<= 1 val 3))
         (mapcar (lambda (a b)
            (abs (- a b)))
          report
          (cdr report))))

(defun read-reports ()
  (mapcar #'(lambda (line)
              (mapcar #'parse-integer (uiop:split-string line)))
          (uiop:read-file-lines *file*)))
