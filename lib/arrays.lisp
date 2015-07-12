(in-package :syswatcher-lib)

(annot.syntax:enable-annot-syntax)

@export
(defmacro doarray (array (index-var value-var) &rest body)
  "Executes body on every item of array. Index and values are bound to provided variable names"
  `(loop
     for ,index-var = 0 then (+ ,index-var 1)
     for ,value-var across ,array
     do (progn
	  ,@body)))
