;; ASDF package description for cl-neo4j              -*- Lisp -*-

(defpackage :cl-neo4j-system (:use :cl :asdf))
(in-package :cl-neo4j-system)

(defsystem cl-neo4j
  :name "neo4j RESTful Client Interface ()"
  :maintainer "Mikhail Novikov <freiksenet@gmail.com>"
  :author "Kevin Raison <last name @ chatsubo dot net>"
  :version "0.3"
  :description "neo4j RESTful Client Interface"
  :long-description "neo4j RESTful Client Interface."
  :depends-on (:alexandria
               :anaphora
               :drakma
               :cl-ppcre
               :cl-json)
  :components ((:file "cl-neo4j-package")
               (:file "globals" :depends-on ("cl-neo4j-package"))
               (:file "utilities" :depends-on ("globals"))
               (:file "conditions" :depends-on ("utilities"))
               (:file "struct" :depends-on ("conditions"))
               (:file "neo4j" :depends-on ("conditions" "struct"))
               (:file "struct-methods" :depends-on ("neo4j"))))
