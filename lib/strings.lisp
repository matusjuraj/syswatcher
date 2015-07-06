(in-package :syswatcher-lib)

(annot.syntax:enable-annot-syntax)

@export
(defmacro concat (&rest args)
  `(concatenate 'string ,@args))

@export
(defun ensure-part (in-string part &key (index 0) (from-end nil))
  (let* ((l (length in-string))
	 (pl (length part))
	 (from (if from-end (- l (+ pl index)) index))
	 (actual (subseq in-string from (+ from pl))))
    (if (string= part actual)
	in-string
	(let ((i (if from-end (- l index) index)))
	  (concat (subseq in-string 0 i) part (subseq in-string i))))))

@export
(defun make-regex (str regex-p)
  (if regex-p str (quote-meta-chars str)))

@export
(defun substr-count (substr target-string &key (regex-p t))
  (/ (length (all-matches (make-regex substr regex-p) target-string)) 2))

@export
(defun replace-and-capture-all (substr target-string replacement &key (regex-p t))
  (let ((rg (make-regex substr regex-p)))
    (values (regex-replace-all rg target-string replacement)
	    (all-matches-as-strings rg target-string))))
