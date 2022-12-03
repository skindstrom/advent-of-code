(defun parse-file ()
  (uiop:read-file-lines "2022/03/input.txt"))

(defun compartment-items (compartment)
  (loop for item across compartment
	with items = (make-hash-table)
	do (setf (gethash item items) (1+ (or (gethash item items) 0)))
	finally (return items)))

(defun rucksak-compartments (rucksak)
  (let ((mid (/ (length rucksak) 2)))
    (cons (subseq rucksak 0 mid)
	  (subseq rucksak mid))))

(defun find-shared-item (items-1 items-2 &rest rest)
  (loop for item being the hash-keys in items-1
	  do (if (reduce (lambda (x items) (and x (gethash item items))) (cons items-2 rest) :initial-value t)
		 (return item))))

(defun item-priority (item)
  (- (char-code item)
     (if (char<= #\a item #\z)
	 96
	 38)))

(defun solution-1 ()
  (reduce #'+
	  (mapcar #'item-priority
		  (mapcar (lambda (rucksack)
			    (let ((compartments (rucksak-compartments rucksack)))
			      (find-shared-item (compartment-items (car compartments))
						(compartment-items (cdr compartments)))))
			  (parse-file)))))


(defun group-rucksacks (rucksacks)
  (loop for idx below (/ (length rucksacks) 3)
	collect (subseq rucksacks (* 3 idx) (* 3 (1+ idx)))))

(defun solution-2 ()
  (reduce #'+
	  (mapcar #'item-priority
		  (mapcar (lambda (group)
			    (apply #'find-shared-item
				   (mapcar #'compartment-items group)))
			  (group-rucksacks (parse-file))))))
