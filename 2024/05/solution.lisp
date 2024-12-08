(use-package :cl-utilities)
(use-package :alexandria)
(defparameter *file* (pathname "input.txt"))

(defun solution-1 ()
  (destructuring-bind (rules updates) (parse-file)
      (reduce (lambda (acc update)
                (if (valid-update-p update rules)
                    (+ acc (middle update))
                    acc))
              updates
              :initial-value 0)))

(defun solution-2 ()
  (destructuring-bind (rules updates) (parse-file)
      (reduce (lambda (acc update)
                (if (not (valid-update-p update rules))
                    (+ acc (middle (fix-update update rules)))
                    acc))
              updates
              :initial-value 0)))

(defun valid-update-p (update rules)
  (labels ((iter (seen update)
             "Returns the seen updates, or nil if invalid"
             (if (null update)
                 t
                 (let ((invalid (gethash (car update) rules)))
                   (when (notany (lambda (i) (member i seen))
                                 invalid)
                     (iter (cons (car update) seen)
                           (cdr update)))))))
    (iter nil update)))

(defun fix-update (update rules)
  (labels ((iter (seen update)
             "Returns the seen updates, or nil if invalid"
             (if (null update)
                 seen
                 (iter (swap (cons (car update)
                                   seen))
                       (cdr update))))
           (swap (seen)
             (let ((invalid (gethash (car seen) rules)))
               (let ((pos (position-if (lambda (s)
                                         (member s invalid))
                                       seen
                                       :from-end t)))
                 (when pos
                   (rotatef (car seen)
                            (elt seen pos)))
                 seen))))
    (iter nil update)))

(defun middle (seq)
  (elt seq (floor (/ (length seq)
                     2))))

(defun parse-file ()
  (destructuring-bind (rules updates) (split-sequence "" (uiop:read-file-lines *file*)  :test #'string=)
    (list (parse-rules rules)
          (parse-updates updates))))

(defun parse-rules (rules)
  "Rules is a hashmap where the key is a page, and the value is the page that it must be printed before"
  (let ((ht (make-hash-table)))
    (loop for rule in rules
          do (destructuring-bind (before after) (mapcar #'parse-integer
                                                        (split-sequence #\|
                                                                        rule
                                                                        :test #'char=))
               (setf (gethash before ht)
                     (cons after (gethash before ht))))
          finally (return ht))))

(defun parse-updates (updates)
  (mapcar (lambda (update)
            (mapcar #'parse-integer
                    (split-sequence #\, update :test #'char=)))
          updates))
