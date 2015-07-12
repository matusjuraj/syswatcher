(in-package :syswatcher-lib)

(annot.syntax:enable-annot-syntax)

@export
(defmacro defsynonym (new-name old-name)
  "Define OLD-NAME to be equivalent to NEW-NAME when used in
the first position of a Lisp form."
  `(defmacro ,new-name (&rest args)
     `(,',old-name ,@args)))
@export
(defsynonym alias defsynonym)

@export
(defmacro once-only ((&rest names) &body body)
  (let ((gensyms (loop for n in names collect (gensym))))
    `(let (,@(loop for g in gensyms collect `(,g (gensym))))
       `(let (,,@(loop for g in gensyms for n in names collect ``(,,g ,,n)))
	  ,(let (,@(loop for n in names for g in gensyms collect `(,n ,g)))
	     ,@body)))))

@export
(defmacro with-gensyms ((&rest names) &body body)
  `(let ,(loop for n in names collect `(,n (gensym (symbol-name ',n))))
     ,@body))

@export
(defun mkstr (&rest args)
  "Changes any lisp token to string"
  (with-output-to-string (s)
    (dolist (a args) (princ a s))))

@export
(defun symb (&rest args)
  "Makes symbol from any lisp token"
  (values (intern (string-upcase (apply #'mkstr args)))))

@export
(defun symb-from (pkg &rest args)
  "Makes symbol from any lisp token, searching in given package"
  (values (intern (string-upcase (apply #'mkstr args)) pkg)))

@export
(defmacro strcase (clause-bind reference-string &body cases)
  "Case with string equality checker. Binds reference-string to variable named in clause-bind and compares with case forms."
  `(let ((,clause-bind ,reference-string))
     (cond
       ,@(loop for case in cases
	       collect
	       (cond
		 ((atom case) `(t ,case))
		 ((null (cdr case)) `(t ,@case))
		 (t `((string= ,clause-bind ,(car case)) ,@(cdr case))))))))
