(in-package :cl-user)
  
(defpackage :syswatcher-lib
  (:use :cl :cl-ppcre :cl-fad :annot.class))

(defpackage :syswatcher
  (:use :cl :syswatcher-lib :cl-ppcre :cl-fad :annot.class))
  
