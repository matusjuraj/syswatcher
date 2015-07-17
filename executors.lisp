(in-package :syswatcher)

(annot.syntax:enable-annot-syntax)

@export
(defparameter *executors* (make-hash-table))

(defmacro defexecutor (name (&rest args) progname progargs
		       &key (progprefix "/usr/bin/"))
  `(setf (gethash ',name *executors*)
	 (lambda (,@args)
	   (external-program:run
	    ,(concat progprefix (if (stringp progname)
				    progname
				    (string-downcase (mkstr progname))))
	    ,progargs))))
  
(defexecutor notify (text) notify-send (list text))

(defexecutor suspend () systemctl '("suspend"))

(defexecutor shutdown () systemctl '("shutdown"))
