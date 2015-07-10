(in-package :syswatcher)

(annot.syntax:enable-annot-syntax)

(defmacro def-cummulative-predicate (name
				     get-data-form
				     ((data-var-name)
				      (&rest data-to-time-body)
				      (&rest moment-predicate-body))
				     &optional (min-time 60000))
  (with-gensyms (time moment-predicate get-time)
    `(let ((,time 0)
	   (,moment-predicate (lambda (,data-var-name)
				,@moment-predicate-body))
	   (,get-time (lambda (,data-var-name)
			,@data-to-time-body)))
       (defun ,name ()
	 (let* ((data ,get-data-form))
	   (if (funcall ,moment-predicate data)
	       (> (incf ,time (funcall ,get-time data)) ,min-time)
	       (and (setf ,time 0) nil))))
       (export ',name))))

(def-cummulative-predicate low-download (get-network-rate 'receive-bytes)
  ((data) ((rate-reading-time-diff data)) ((< (rate-reading-rate data) 1000000))))
