(use-package :alexandria)
(defparameter *file* (pathname "input.txt"))

(defun solution-1 ()
  "How many antinodes are there on the map? An antinode appears between two antennas.
They appear when one antenna is twice as far away as the other, in a perfect line"
  (destructuring-bind (map antennas) (parse-file)
    (length (remove-duplicates
             (mapcan (curry #'antinodes map) antennas)
             :test #'equal))))

(defun solution-2 ()
  (destructuring-bind (map antennas) (parse-file)
    (length (remove-duplicates
             (remove-if-not (curry #'valid-pos-p map)
                            (mapcan (lambda (antenna)
                                      (antinodes map antenna :resonant-harmonics t))
                                    antennas))
             :test #'equal))))

(defun valid-pos-p (map pos)
  (and (< -1 (y pos) (array-dimension map 0))
       (< -1 (x pos) (array-dimension map 1))))

(defun antinodes (map antenna &key (resonant-harmonics nil))
  "Retruns all antinodes for a given antenna, regardless if it's within bounds"
  (labels ((iter (antinodes curr rest)
             (if (null rest)
                 antinodes
                 (iter (append antinodes
                               (mapcan (lambda (pos)
                                         (let ((diff (pos- curr pos)))
                                           (if resonant-harmonics
                                               (append (harmonics nil curr diff #'pos+)
                                                       (harmonics nil pos diff #'pos-))
                                               (list (pos+ curr diff)
                                                     (pos- pos diff)))))
                                       rest))
                       (car rest)
                       (cdr rest))))
           (harmonics (antinodes curr diff fn)
             (if (not (valid-pos-p map curr))
                 antinodes
                 (harmonics (cons curr antinodes)
                            (funcall fn curr diff)
                            diff
                            fn))))
    (let ((positions (antenna-positions antenna)))
      (remove-if-not (curry #'valid-pos-p map)
                     (iter nil (car positions) (cdr positions))))))

(defun pos+ (a b)
  (list (+ (y a)
           (y b))
        (+ (x a)
           (x b))))
(defun pos- (a b)
  (list (- (y a)
           (y b))
        (- (x a)
           (x b))))

(defun x (pos)
  (cadr pos))

(defun y (pos)
  (car pos))

(defun antenna-char (a)
  (car a))

(defun antenna-positions (a)
  (cdr a))


(defun parse-file ()
  "Returns the map, and the antennas as an alist"
  (let* ((lines (uiop:read-file-lines *file*))
         (arr (make-array (list (length lines)
                                (length (first lines)))
                          :initial-contents lines))
         (antennas (make-hash-table :test #'equal)))
    (dotimes (y (array-dimension arr 0))
      (dotimes (x (array-dimension arr 1))
        (let ((char (aref arr y x)))
          (when (char/= #\. char)
            (setf (gethash char antennas)
                  (cons (list y x)
                        (gethash char antennas)))))))
    (list arr (hash-table-alist antennas))))
