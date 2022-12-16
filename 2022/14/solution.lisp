;;;; Problem: Sand falling down one unit at a time.
;;;; Keep falling until it hits a stone or sand. Then checks diagonally left then right.
;;;; If not possible, stays where it's at.
;;;; Done when a piece of sand will fall forever
;;;;
;;;; Input: Ranges
;;;; Output: Locations of sand
;;;;
;;;; Thoughts:
;;;; 1. We could use a hashmap of pos -> type. Would need to find max y to account for bottom of map
;;;; 2. We could use a 2d array. We need to normalize the access.
;;;;
;;;; Pro 1: No setup required. No normalization required. Only store what's needed. (Sparse)
;;;; Con 1: Costlier access. Shouldn't be too bad though.
;;;; Pro 2: Fast memory access.
;;;; Con 2: Possibly lots of unused memory. Need to normalize ranges
;;;;
;;;; Next step:
;;;; 1. Can go down? Do so
;;;; 2. Can go down+left? Do so
;;;; 3. Can go down+right? Do so
;;;; 4. Else stop

(require :uiop)
(require :cl-utilities)

(defparameter *file* "2022/14/input.txt")

(defclass pos ()
  ((x :initarg :x
      :type number
      :accessor pos-x)
   (y :initarg :y
      :type number
      :accessor pos-y)))

(defmethod print-object ((obj pos) stream)
  (print-unreadable-object (obj stream :type t)
    (with-accessors ((x pos-x)
                     (y pos-y))
        obj
      (format stream "~a:~a" x y))))


(defun pos-hash (pos)
  (sxhash (cons (pos-x pos)
		(pos-y pos))))

(defun pos= (a b)
  (and a b
       (= (pos-x a)
	  (pos-x b))
       (= (pos-y a)
	  (pos-y b))))

(defun make-pos (x y)
  (make-instance 'pos
		 :x x
		 :y y))

(defun pos-down (pos)
  (make-pos (pos-x pos)
	    (1+ (pos-y pos))))

(defun pos-diag-left (pos)
  (make-pos (1- (pos-x pos))
	    (1+ (pos-y pos))))

(defun pos-diag-right (pos)
  (make-pos (1+ (pos-x pos))
	    (1+ (pos-y pos))))

(defun pos-in-range (pos tiles)
  (> (tiles-bottom tiles) (pos-y pos)))

(defun pos-valid (pos tiles)
  (not (tiles-content tiles pos)))

(defun pos-next (pos tiles)
  (flet ((is-valid (pos) (pos-valid pos tiles)))
    (find-if #'is-valid
	     (list (pos-down pos)
		   (pos-diag-left pos)
		   (pos-diag-right pos)
		   pos))))
		   

(defclass tiles ()
  ((bottom :type number
	   :initform 0
	   :accessor tiles-bottom)
   (data :type hash-table
	 :initform (make-hash-table :test #'pos= :hash-function #'pos-hash)
	 :accessor tiles-data)))

(defun make-tiles ()
  (make-instance 'tiles))

(defmacro tiles-content (tiles pos)
  `(if (= (pos-y ,pos) (tiles-bottom ,tiles))
       :rock
       (gethash ,pos (tiles-data ,tiles))))

(defun tiles-add-sand (tiles pos)
  (setf (tiles-content tiles pos) :sand))

(defun tiles-add-rock (tiles pos)
  (setf (tiles-bottom tiles) (max (tiles-bottom tiles)
				  (pos-y pos)))
  (setf (tiles-content tiles pos) :rock))

(defun pos-range (pos-start pos-end)
  (let ((x-start (min (pos-x pos-start)
		      (pos-x pos-end)))
	(x-end (max (pos-x pos-start)
		    (pos-x pos-end)))
	(y-start (min (pos-y pos-start)
		      (pos-y pos-end)))
	(y-end (max (pos-y pos-start)
		    (pos-y pos-end))))
    (loop for x from x-start to x-end
	  append (loop for y from y-start to y-end
		       collect (make-pos x y)))))

(defun tiles-count-sand (tiles)
  (let ((count 0))
    (maphash #'(lambda (key value)
		 (declare (ignore key))
		 (if (eq value :sand)
		     (setf count (1+ count))))
	     (tiles-data tiles))
    count))

(defun parse-line (string)
  (if (not string)
      nil
      (cons (make-pos
	     (parse-integer (subseq string 0 (position #\, string)))
	     (parse-integer (subseq string (1+ (position #\, string)) (position #\space string))))
	    (parse-line (if (position #\> string)
			    (subseq string (+ 2 (position #\> string)))
			    nil)))))

(defun parse-input (path)
  (mapcar #'parse-line (uiop:read-file-lines path)))

(defun path-pos-ranges (path)
  (loop for start in (subseq path 0 (1- (length path)))
	and end in (subseq path 1)
	append (pos-range start end)))

(defun pos-can-stop (pos tiles)
  (and (pos-in-range pos tiles)
       (not (null (tiles-content tiles (pos-down pos))))))

(defun tiles-next-sand-pos (tiles)
  (loop with pos = (make-pos 500 0)
	while (and pos (pos-in-range pos tiles))
	do (let ((next-pos (pos-next pos tiles)))
	     (if (pos= pos next-pos)
		 (return (when (pos-can-stop pos tiles) pos))
		 (setf pos next-pos)))))

(defun tiles-build (paths)
  (let ((tiles (make-tiles)))
    (map nil
	 #'(lambda (pos) (tiles-add-rock tiles pos))
	 (reduce #'append (mapcar #'path-pos-ranges paths)))
    tiles))  

(defun solution-1 ()
  (let ((tiles (tiles-build (parse-input *file*))))
    (loop with pos = (tiles-next-sand-pos tiles)
	  while pos
	  do (tiles-add-sand tiles pos)
	     (setf pos (tiles-next-sand-pos tiles)))
    (tiles-count-sand tiles)))

;; There's a floor at 2+ current bottom that's infinite
;; That means that no sand ever disappears and it'll always build all the way up
;; What changes are required?
;; Pos-can-stop needs to change to be true if = bottom +2
;; Bottom needs to be updated too to increase by two
(defun solution-2 ()
  (let ((tiles (tiles-build (parse-input *file*))))
    (setf (tiles-bottom tiles) (+ 2 (tiles-bottom tiles)))
    (loop with pos = (tiles-next-sand-pos tiles)
	  while pos
	  do (tiles-add-sand tiles pos)
	     (setf pos (tiles-next-sand-pos tiles)))
    (tiles-count-sand tiles)))
