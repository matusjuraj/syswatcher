(in-package :syswatcher)

(annot.syntax:enable-annot-syntax)

@export
(defun main (&key when then)
  (let ((when-symbol (symb-from :syswatcher when))
	(then-symbol (symb-from :syswatcher then)))
    (if (and (fboundp when-symbol) (fboundp then-symbol))	
	(loop
	  do (if (funcall (symbol-function when-symbol))
		 (funcall (symbol-function then-symbol))
		 (sleep 1)))
	(format t "Inexistent function (~s OR ~s)" when then))))
