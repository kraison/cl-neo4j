;; ASDF package description for cl-neo4j              -*- Lisp -*-

(defpackage :cl-neo4j-system (:use :cl :asdf))
(in-package :cl-neo4j-system)

(defsystem cl-neo4j
  :name "neo4j RESTful Client Interface ()"
  :maintainer "Mikhail Novikov <freiksenet@gmail.com>"
  :author "Kevin Raison <last name @ chatsubo dot net>"
  :version "0.3"
  :description "neo4j RESTful Client Interface"
  :long-description "neo4j RESTful Client Interface and higher order extensible CL wrapper for it."
  :depends-on (:alexandria
               :anaphora
               :split-sequence
               :drakma
               :babel
               :cl-json
               :cl-ppcre)
  :components
  ((:module "src"
            :serial t
            :components
            ((:file "package")
             (:file "utilities")
             (:file "conditions")
             (:file "requests")
             (:file "restapi")
             (:file "wrapper")
             #+nil(:module "wrapper"
                      :serial t
                      :components
                      (:file "protocol")
                      (:file "api")
                      (:file "standard-classes"))))))

(defsystem cl-neo4j.tests
  :depends-on  (:cl-neo4j
                :fiveam)
  :components
  ((:module "test"
            :serial t
            :components
            ((:file "package")
             (:file "util")
             (:file "main")
             (:file "restapi")
             (:file "wrapper")))))