(defpackage :syswatcher-lib-asdf (:use :asdf :cl))
(in-package :syswatcher-lib-asdf)

(defsystem syswatcher-lib
  :name "syswatcher-lib"
  :author "Juraj Matus <matus.juraj@yandex.com>"
  :version "1.0"
  :maintainer "Juraj Matus <matus.juraj@yandex.com>"
  :licence "BSD"
  :description "Personal set of libraries."
  :long-description ""
  :depends-on ("cl-annot" "cl-ppcre" "cl-fad")
  :components
  ((:file "packages")
   (:file "macros" :depends-on ("packages"))
   (:file "functional" :depends-on ("packages" "macros"))
   (:file "lists" :depends-on ("packages" "macros"))
   (:file "maps" :depends-on ("packages" "macros" "lists"))
   (:file "strings" :depends-on ("packages" "macros"))
   (:file "arrays" :depends-on ("packages" "macros"))
   (:file "files" :depends-on ("packages" "macros" "strings"))))
