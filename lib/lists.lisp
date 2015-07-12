(in-package :syswatcher-lib)

(annot.syntax:enable-annot-syntax)

@export
(defun last-item (lst)
  "Gets last item of the list"
  (car (last lst)))

@export
(defmacro pop-last (lst)
  "Gets and destructively removes the last item of the list"
  `(prog1
    (car (last ,lst))
    (setf (cdr (last ,lst 2)) nil)))

@export
(defmacro pop-n (lst n)
  "Destructively pops n items from the list and returns them as sublist"
  (labels ((pop-n-helper (listname n)
	     (if (> n 0)
		 (append (list `(add (pop ,listname)))
			 (pop-n-helper listname (1- n))))))
    `(with-list-builder-adder (add)
       ,@(pop-n-helper lst n))))

@export
(defmacro push-end (item lst)
  "Pushes an item to the end of existing list"
  `(setq ,lst (nconc ,lst (list ,item))))

@export
(defun append-item (lst item)
  "Appends an item to the end of list"
  (append lst (list item)))

@export
(defmacro to-list (lst)
  "Returns the list itself or a list containing only the given item"
  `(if (listp ,lst) ,lst (list ,lst)))

@export
(defun longer (x y)
  "Checks if x is longer list than y"
  (labels ((compare (x y)
                    (and (consp x)
                         (or (null y)
                             (compare (cdr x) (cdr y))))))
    (if (and (listp x) (listp y))
        (compare x y)
      (> (length x) (length y)))))

@export
(defun chunk (source n)
  "Transforms a list so that items are grouped in sublists of length n"
  (if (zerop n) (error "zero length"))
  (labels ((rec (source acc)
	     (let ((rest (nthcdr n source)))
	       (if (consp rest)
		   (rec rest (cons (subseq source 0 n) acc)) (nreverse (cons source acc))))))
    (if source (rec source nil) nil)))

@export
(defun flatten (x)
  "Transforms recursive list into a flat one"
  (labels ((rec (x acc)
	     (cond ((null x) acc)
		   ((atom x) (cons x acc))
		   (t (rec (car x) (rec (cdr x)
					acc))))))
    (rec x nil)))

@export
(defmacro random-list (variable &optional (length 20) &key (min 0) (max 100))
  "Generates a list filled with random numbers"
  `(symbol-value
    (defparameter ,variable
      (loop for i from 1 to ,length collect
            (+ ,min (random (- ,max ,min)))))))

(let ((ignoring-1+ (lambda (num &rest ignoring)
		     (declaim (ignore ignoring))
		     (1+ num))))
  @export
  (defun group (sequence &key (key #'identity)
			   (group-initial-value 0) (group-accumulator ignoring-1+))
    "Groups items by equality after key function is applied and returns a list of sublists, where car is a value and cdr is a result of group accumulator (number of occurrences by default)"
    (let ((h (make-hash-table :test #'equal))
	  (ret (list)))
      (dolist (item sequence)
	(let* ((k (funcall key item))
	       (group (gethash k h)))
	  (if (null group)
	      (setf group group-initial-value))
	  (setf group (funcall group-accumulator group item))
	  (setf (gethash k h) group)))
      (maphash (lambda (key val)
		 (push (cons key val) ret)) h)
      (nreverse ret))))

@export
@export-accessors
(defclass list-builder ()
  ((f :type 'list :initform nil :reader get-list)
   (l :type 'list :initform nil)))

(defgeneric add-item (this item))

@export
(defmethod add-item ((this list-builder) item)
  (if (consp (slot-value this 'f))
      (setf (slot-value this 'l) (setf (cdr (slot-value this 'l)) (list item)))
      (progn
	(setf (slot-value this 'f) (list item))
	(setf (slot-value this 'l) (slot-value this 'f)))))

@export
(defmacro with-list-builder-adder ((adder) &body body)
  "Provides efficient way to build lists without the need for reversion. Bound variable is a function to add an item to the end of the list. The resulting list is returned"
  (with-gensyms (lb)
    `(let ((,lb (make-instance 'list-builder)))
       (flet ((,adder (item)
		(add-item ,lb item)))
	 ,@body
	 (get-list ,lb)))))
