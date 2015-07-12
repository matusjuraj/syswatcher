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
  "Copies input stream into output stream."
  (buffered-read (input buffer total-length num-read)
      (buffer-length :element-type element-type :initial-element nil)
      (write-sequence buffer output :end num-read)))

@export
(defun stream-to-string (input)
  "Returns stream contents as string"
  (with-output-to-string (out)
    (pipe input out)))

@export
(defun file-to-string (path)
  "Returns file contents as string"
  (with-open-file (input path)
    (stream-to-string input)))

@export
(defun stream-to-byte-array (input)
  "Returns stream contents as byte array"
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
(defun file-to-byte-array (path)
  "Returns file contents as byte array"
  (with-open-file (input path :element-type '(unsigned-byte 8))
    (stream-to-byte-array input)))
