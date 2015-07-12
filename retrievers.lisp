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

(defun get-network-stats ()
  (destructuring-bind (header1 header2 &rest numbers) (cl-ppcre:split "\\n+" (syswatcher-lib:file-to-string #P"/proc/net/dev"))
    (let ((columns (parse-headers header1 header2))
	  (num-data (mapcar (lambda (row)
			      (split-nonempty row "[\\s:]+")) numbers)))
      (apply #'mapcar (lambda (name &rest data)
			`(,name . ,(join data))) columns num-data))))

@export-structure
(defstruct reading
  (name 'unknown :type symbol))

@export-structure
(defstruct (rate-reading
	    (:include reading))
  (diff 0.0 :type number)
  (time-diff 0.0 :type number)
  (rate 0.0 :type number))

(defmacro def-difference-retriever (name (&rest args)
				    name-getter
				    data-retriever
				    ((data-var-name) &body data-to-value-body))
  (with-gensyms (last-read last-data data-to-value)
    `(let ((,last-read (get-internal-real-time))
	   (,last-data ,data-retriever)
	   (,data-to-value (lambda (,data-var-name ,@args)
			     ,@data-to-value-body)))
       (defun ,name (,@args)
	 (let* ((prev-last-read ,last-read)
		(prev-last-data ,last-data)
		(last-read (setf ,last-read (get-internal-real-time)))
		(last-data (setf ,last-data ,data-retriever))
		(prev-value (funcall ,data-to-value prev-last-data ,@args))
		(value (funcall ,data-to-value last-data ,@args))
		(elapsed-time (- last-read prev-last-read)))
	   (if (and (numberp prev-value) (numberp value) (> elapsed-time 0))
	       (make-rate-reading :name ,name-getter
				  :time-diff elapsed-time
				  :diff (- value prev-value)
				  :rate (float (/ (- value prev-value) (/ elapsed-time 1000))))
	       (make-rate-reading))))
       (export ',name))))

(def-difference-retriever get-network-rate (prop)
			  prop
			  (get-network-stats)
    ((data) (let ((cell (assoc prop data)))
	      (if (null cell)
		  nil
		  (cdr cell)))))
