(in-package :syswatcher)

(annot.syntax:enable-annot-syntax)

(defun get-network-rate()
  (cl-ppcre:split "\\n" (syswatcher-lib:to-string #P"/proc/net/dev")))
