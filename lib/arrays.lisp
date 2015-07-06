(in-package :syswatcher-lib)

(annot.syntax:enable-annot-syntax)

@export
(defmacro doarray (array (index-var value-var) &rest body)
  `(loop
     for ,index-var = 0 then (+ ,index-var 1)
     for ,value-var across ,array
     do (progn
	  ,@body)))
