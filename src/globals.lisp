(in-package #:cl-neo4j)

(defvar *neo4j-host* "localhost")
(defvar *neo4j-port* 7474)

(defmacro with-neo4j-database ((host port) &rest body)
  `(let ((*neo4j-host* ,host)
         (*neo4j-port* ,port))
     ,@body))
