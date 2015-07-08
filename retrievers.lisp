(in-package :syswatcher)

(annot.syntax:enable-annot-syntax)

(defun split-nonempty (str &optional (regex "\\s+"))
  (remove-if (lambda (col)
	       (= (length col) 0))
	     (cl-ppcre:split regex str)))

(defun parse-headers (header1 header2)
  (let* ((raw-columns (mapcar (lambda (header)
				(cl-ppcre:split "\\|" header)) (list header1 header2)))
	 (columns (mapcar (lambda (first-row second-row)
			    (list (split-nonempty first-row) (split-nonempty second-row))) (car raw-columns) (cadr raw-columns))))
    (flet ((combine-words (two-rows-list)
	     (let* ((first-part-raw (caar two-rows-list))
		    (first-part (if (string= (subseq first-part-raw (1- (length first-part-raw))) "-")
				    (subseq first-part-raw 0 (1- (length first-part-raw)))
				    (concat first-part-raw "-"))))
	       (mapcar (lambda (second-part)
			 (symb (concat first-part second-part))) (cadr two-rows-list)))))
      (mapcan (lambda (two-rows-column)
		(combine-words two-rows-column)) columns))))

(defun to-num (str-or-int)
  (if (numberp str-or-int)
      str-or-int
      (parse-integer str-or-int :junk-allowed t)))

(defun join (data)
  (reduce (lambda (left right)
	    (let ((leftnum (to-num left))
		  (rightnum (to-num right)))
	      (if (and (numberp leftnum) (numberp rightnum))
		  (+ leftnum rightnum)
		  (concat left ", " right)))) data))

(defvar last-read)
(defvar last-stats)

@export
(defun get-network-stats ()
  (setf last-read (get-internal-real-time))
  (destructuring-bind (header1 header2 &rest numbers) (cl-ppcre:split "\\n+" (syswatcher-lib:to-string #P"/proc/net/dev"))
    (let ((columns (parse-headers header1 header2))
	  (num-data (mapcar (lambda (row)
			      (split-nonempty row "[\\s:]+")) numbers)))
      (setf last-stats (apply #'mapcar (lambda (name &rest data)
					 `(,name . ,(join data))) columns num-data)))))

(get-network-stats)

@export
(defun get-network-rate (prop)
  (let* ((prev-last-read last-read)
	 (prev-last-stats last-stats)
	 (stats (get-network-stats))
	 (new-item (assoc prop stats))
	 (old-item (assoc prop prev-last-stats)))
    (cond
      ((or (null new-item) (null old-item)) `(:prop ,prop :val nil))
      ((not (and (numberp (cdr new-item)) (numberp (cdr old-item)))) `(:prop ,prop :val ,(cdr new-item)))
      (t (let* ((elapsed-time (- last-read prev-last-read))
		(diff (- (cdr new-item) (cdr old-item)))
		(rate (if (= elapsed-time 0) nil (/ diff (/ elapsed-time 1000)))))
	   `(:prop ,prop :val ,diff :time-delta ,elapsed-time :rate-per-second ,(float rate)))))))
