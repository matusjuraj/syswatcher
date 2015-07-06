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
(defmacro func (name arg-form &body body)
  (if name
      `(defun ,name ,arg-form ,@body)
      `(lambda ,arg-form ,@body)))

@export
(defun mkstr (&rest args)
  (with-output-to-string (s)
    (dolist (a args) (princ a s))))

@export
(defun symb (&rest args)
  (values (intern (string-upcase (apply #'mkstr args)))))

@export
(defun symb-from (pkg &rest args)
  (values (intern (string-upcase (apply #'mkstr args)) pkg)))

(export 'it)
  
@export
(defmacro aif (cond true false)
  `(let ((it ,cond))
     (if it ,true ,false)))

@export
(defmacro awhen (test-form &body body)
  `(aif ,test-form
	(progn ,@body)))

@export
(defmacro awhile (expr &body body)
  `(do ((it ,expr ,expr))
       ((not it))
     ,@body))

@export
(defmacro aand (&rest args)
  (cond ((null args) t)
	((null (cdr args)) (car args))
	(t `(aif ,(car args) (aand ,@(cdr args))))))

@export
(defmacro acond (&rest clauses)
  (if (null clauses) nil
      (let ((cll (car clauses))
	    (sym (gensym)))
	`(let ((,sym ,(car cll)))
	   (if ,sym
	       (let ((it ,sym)) ,@(cdr cll))
	       (acond ,@(cdr clauses)))))))

(defun compose-helper (args functions)
  (if (cdr functions)
      `(,(pop functions) ,(compose-helper args functions))
    `(apply #',(pop functions) ,args)))

@export
(defmacro compose (&rest functions)
  "Macro to create an anonymous function, which calls functions in arguments
recursively, where the last one is called as the first, and is the only one
allowed to have more than one input parameter"
  `(lambda (&rest args) ,(compose-helper 'args functions)))

@export
(defmacro strcase (clause-bind reference-string &body cases)
  `(let ((,clause-bind ,reference-string))
     (cond
       ,@(loop for case in cases
	       collect
	       (cond
		 ((atom case) `(t ,case))
		 ((null (cdr case)) `(t ,@case))
		 (t `((string= ,clause-bind ,(car case)) ,@(cdr case))))))))
