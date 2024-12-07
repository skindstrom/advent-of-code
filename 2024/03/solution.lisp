(use-package :cl-ppcre)
(defparameter *file* (pathname "input.txt"))

(defun solution-1 ()
  (reduce (lambda (sum match)
            (+ sum (* (first match)
                      (second match))))
          (mapcar #'parse-match
                  (get-matches (read-file)))
          :initial-value 0))

(defun solution-2 ()
  (reduce (lambda (acc match)
            (destructuring-bind (state sum) acc
                (cond ((eq match :do) (list :do sum))
                      ((eq match :dont) (list :dont sum))
                      ((eq state :do) (list :do (+ sum
                                                  (* (first match)
                                                     (second match)))))
                      (t acc))))
          (mapcar #'parse-match
                  (get-matches (read-file)
                               :with-dos t))
          :initial-value '(:do 0)))

(defun parse-match (match)
  (cond ((string= match "do()") :do)
        ((string= match "don't()") :dont)
        (t (remove-if #'null
             (mapcar (lambda (str)
                       (parse-integer str :junk-allowed t))
                     (uiop:split-string (subseq match 3)
                                        :separator '(#\( #\) #\,)))))))

(defun get-matches (str &key (with-dos nil))
  (ppcre:all-matches-as-strings (if with-dos
                                    "(do\\(\\)|don't\\(\\)|mul\\(\\d{1,3},\\d{1,3}\\))"
                                    "mul\\(\\d{1,3},\\d{1,3}\\)")
                                str))

(defun read-file ()
  (uiop:read-file-string *file*))
