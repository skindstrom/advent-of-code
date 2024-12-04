(defparameter *file* (pathname "input.txt"))
(defun solution-1 ()
  (count-if #'is-safe-recur (read-reports)))

(defun is-safe-recur (report)
  (labels ((iter (report direction)
             (destructuring-bind (a b &rest rest) report
               (and (case direction
                      (:up (< (car report) (cadr report)))
                      (:down (> (car report) (cadr report))))
                    (<= 1 (abs (- a b)) 3)
                    (if (null rest)
                        t
                        (iter (cons b rest) direction)
                        )))))
    (iter report (if (< (car report) (cadr report))
                     :up
                     :down))))

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
