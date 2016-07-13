(in-package #:cl-user)
(asdf:defsystem neato
  :author "Janne Pakarinen <gingeralesy@gmail.com>"
  :description "For experimenting with all the things."
  :components ((:file "package")
               (:file "main")
               (:file "neato"))
  :depends-on (:verbose
               :qtools
               :qtcore
               :qtgui))
