(in-package :syswatcher)

(annot.syntax:enable-annot-syntax)

(defun val-to-fn (val &optional args)
  (if (functionp val)
      (if args
	  (lambda ()
	    (apply val args))
	  val)
      nil))

(defun get-fn (descriptor hash-map)
  (if (listp descriptor)
      (let ((fn-symbol (car descriptor))
	    (args (cdr descriptor)))
	(val-to-fn (gethash (symb-from :syswatcher fn-symbol) hash-map) args))
      (val-to-fn (gethash (symb-from :syswatcher descriptor) hash-map))))

@export
(defun main (&key when then)
  (in-package :syswatcher)
  (let ((when-fn (get-fn when *predicates*))
	(reset-fn (get-fn when *resets*))
	(then-fn (get-fn then *executors*)))
    (cond
      ((or (null when-fn) (null reset-fn)) (format t "Predicate ~s doesn't exist ~%" when))
      ((null then-fn) (format t "Executor ~s doesn't exist ~%" then))
      (t (funcall reset-fn)
	 (loop
	   do (if (funcall when-fn)
		  (progn
		    (funcall then-fn)
		    (return))
		  (sleep 1)))))))
