(in-package :syswatcher-lib)

(annot.syntax:enable-annot-syntax)

@export
(defmacro concat (&rest args)
  "Concatenates an arbitrary number of strings"
  `(concatenate 'string ,@args))

@export
(defun ensure-part (in-string part &key (index 0) (from-end nil))
  "Checks if strings contains a substring at given index. If not, it's added there"
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
  "Returns a regular expression itself or a new regular expression matching the given string"
  (if regex-p str (quote-meta-chars str)))

@export
(defun substr-count (substr target-string &key (regex-p t))
  "Returns a number of occurrences of substring in string"
  (/ (length (all-matches (make-regex substr regex-p) target-string)) 2))

@export
(defun replace-and-capture-all (substr target-string replacement &key (regex-p t))
  "Returns a string with replacements applied and all replaced groups as a second value"
  (let ((rg (make-regex substr regex-p)))
    (values (regex-replace-all rg target-string replacement)
	    (all-matches-as-strings rg target-string))))
