(defpackage :syswatcher-asdf (:use :asdf :cl))
(in-package :syswatcher-asdf)

(defsystem syswatcher
  :name "syswatcher"
  :author "Juraj Matus <matus.juraj@yandex.com>"
  :version "1.0"
  :maintainer "Juraj Matus <matus.juraj@yandex.com>"
  :licence "GNU GPL v2.0"
  :description "Watcher of system statistics capable of running commands when a condition is satisfied."
  :long-description ""
  :depends-on ("cl-annot" "cl-ppcre" "cl-fad")
  :components
  ((:file "packages")
   (:file "main" :depends-on ("packages"))))