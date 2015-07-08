(in-package :syswatcher)

(annot.syntax:enable-annot-syntax)

(defun join-rows (word1 word2 were)
  (if (string= (subseq word1 (1- (length word1))) "-")
      (values (concat (subseq word1 0 (1- (length word1))) word2) t (push word2 were))
      (values (concat word1 " " word2) (member word2 were) (push word2 were))))

(defun get-column-names (row1 row2)
  (let ((names '()))
    (labels ((take-word (it1 it2 were acc)
	       (if (or (null it1) (null it2))
		   acc
		   (multiple-value-bind (word move-to-next new-were)
		       (join-rows (car it1) (car it2) were)
		     (take-word (if move-to-next (cdr it1) it1) (cdr it2)
				new-were (cons word acc))))))
      (take-word row1 row2 '() '()))))
    

@export
(defun get-network-rate()
  (let* ((all
	  (mapcar (lambda (row)
		    (remove-if (lambda (col)
				 (= (length col) 0))
			       (cl-ppcre:split "[\\|\\s:]+" row)))
		  (cl-ppcre:split "\\n+" (syswatcher-lib:to-string #P"/proc/net/dev"))))
	 (row1 (pop all))
	 (row2 (pop all)))
    (get-column-names row1 row2)))
    
