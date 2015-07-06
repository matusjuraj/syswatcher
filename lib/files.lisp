(in-package :syswatcher-lib)

(annot.syntax:enable-annot-syntax)

@export
(defsynonym read-dir list-directory)
@export
(defsynonym walk-dir walk-directory)

(defmacro buffered-read ((input buffer total-length num-read)
			 (buffer-length &rest buffer-spec) &rest body)
  `(let ((,buffer (make-array ,buffer-length ,@buffer-spec))
	 (,total-length 0))
     (loop for ,num-read = (read-sequence ,buffer ,input)
	   do (progn
		,@body
		(incf ,total-length ,num-read)
		(when (< num-read ,buffer-length)
		 (return ,total-length))))))

@export
(defun pipe (input output &key (buffer-length 4096) (element-type t))
  (buffered-read (input buffer total-length num-read)
      (buffer-length :element-type element-type :initial-element nil)
      (write-sequence buffer output :end num-read)))

@export
(defmethod to-string ((input stream))
  (with-output-to-string (out)
    (pipe input out)))

@export
(defmethod to-string ((file pathname))
  (with-open-file (input file)
    (to-string input)))

@export
(defmethod to-byte-array ((input stream))
  (let* ((buffer-length 16536)
	 (all (make-array buffer-length :element-type '(unsigned-byte 8)
					:initial-element 0
					:adjustable t
					:fill-pointer 0)))
    (buffered-read (input buffer total-length num-read)
	(buffer-length :element-type '(unsigned-byte 8)
		       :initial-element 0)
	(adjust-array all (+ total-length num-read)
		      :fill-pointer (+ total-length num-read))
	(setf (subseq all total-length (+ total-length num-read))
	      (subseq buffer 0 num-read)))
    all))

@export
(defmethod to-byte-array ((file pathname))
  (with-open-file (input file :element-type '(unsigned-byte 8))
    (to-byte-array input)))
