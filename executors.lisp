(in-package :syswatcher)

(annot.syntax:enable-annot-syntax)

(defmacro defexecutor (name (&rest args) progname progargs
		       &key (progprefix "/usr/bin/"))
  `(progn
     (defun ,name (,@args)
       (external-program:run
	,(concat progprefix (if (stringp progname)
				progname
				(string-downcase (mkstr progname))))
	,progargs)
       (export ',name))))
  
(defexecutor notify (text) notify-send (list text))

(defexecutor suspend () systemctl '("suspend"))

(defexecutor shutdown () systemctl '("shutdown"))
