(in-package :syswatcher-lib)

(annot.syntax:enable-annot-syntax)

@export
(defmacro hashmap (&rest pairs)
  "Usage: (hashmap key1 val1 key2 val2 key3 val3 ...)"
  (with-gensyms (map)
    (labels ((get-key-val-pairs (list)
	       (let ((key (car list))
		     (val (cadr list)))
		 (if (or (null key) (null val))
		     nil
		     (cons `(setf (gethash ,key ,map 10) ,val)
			   (get-key-val-pairs (cddr list)))))))
      `(let ((,map (make-hash-table :test 'equal)))
	 ,@(get-key-val-pairs pairs)
	 ,map))))

@export
(defmacro dohash (map (key val) &rest body)
  `(loop for ,key being the hash-keys in ,map using (hash-value ,val) do
	 ,@body))
