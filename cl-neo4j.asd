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
               :split-sequence
               :drakma
               :cl-json)
  :components
  ((:module "src"
            :serial t
            :components
            ((:file "package")
             (:file "globals")
             (:file "utilities")
             (:file "conditions")
             (:file "restapi")
             (:file "wrapper")))))
