(use-package :alexandria)
(defparameter *file* (pathname "input.txt"))

(defun solution-1 ()
  (reduce (lambda (sum calibration)
            (if (valid-calibration-p calibration)
                (+ sum (result calibration))
                sum))
          (parse-file)
          :initial-value 0))

(defun solution-2 ()
  (reduce (lambda (sum calibration)
            (if (valid-calibration-p calibration :allow-concat t)
                (+ sum (result calibration))
                sum))
          (parse-file)
          :initial-value 0))

(defun valid-calibration-p (calibration &key (allow-concat nil))
  "Returns t if the calibration is valid, nil otherwise"
  (labels ((iter (curr parameters)
             (if (null parameters)
                 (= (result calibration) curr)
                 (or (iter (+ curr
                              (car parameters))
                           (cdr parameters))
                     (iter (* curr
                              (car parameters))
                           (cdr parameters))
                     (and allow-concat
                          (iter (parse-integer (format nil "~a~a" curr (car parameters)))
                                (cdr parameters)))))))
    (let ((parameters (parameters calibration)))
      (iter (car parameters)
            (cdr parameters)))))

(defun result (calibration)
  (car line))

(defun parameters (calibration)
  (cdr line))

(defun parse-file ()
  (mapcar (lambda (line)
            (remove-if #'null
                       (mapcar (lambda (val)
                                 (parse-integer val :junk-allowed t))
                               (uiop:split-string line :separator '(#\space #\:)))))
          (uiop:read-file-lines *file*)))
