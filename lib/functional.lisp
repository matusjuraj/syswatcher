(in-package :syswatcher-lib)

(annot.syntax:enable-annot-syntax)

@export
(defmacro compose (&rest args)
  (with-gensyms (args-name)
    (labels ((compose-recurse (fns)
	       (if (cdr fns)
		   `(,(car fns) ,(compose-recurse (cdr fns)))
		   `(apply #',(car fns) ,args-name))))
      `(lambda (&rest ,args-name)
	 ,(compose-recurse args)))))

@export
(defun curry (fn &rest args)
  (lambda (&rest new-args)
    (apply fn (append args new-args))))
			 
