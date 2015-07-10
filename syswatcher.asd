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
   (:file "main" :depends-on ("packages" "predicates"))
   (:file "retrievers" :depends-on ("packages" "lib/files"))
   (:file "predicates" :depends-on ("packages" "retrievers"))
   (:file "lib/macros" :depends-on ("packages"))
   (:file "lib/functional" :depends-on ("packages" "lib/macros"))
   (:file "lib/lists" :depends-on ("packages" "lib/macros"))
   (:file "lib/maps" :depends-on ("packages" "lib/macros" "lib/lists"))
   (:file "lib/strings" :depends-on ("packages" "lib/macros"))
   (:file "lib/arrays" :depends-on ("packages" "lib/macros"))
   (:file "lib/files" :depends-on ("packages" "lib/macros" "lib/strings"))))
   

