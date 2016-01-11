(in-package #:cl-neo4j)

(defclass cypher-query ()
  ((statement :accessor statement :initform "" :initarg :statement)
   (properties :accessor properties :initform nil :initarg :properties)))

(defmethod structure-cypher-query ((q cypher-query))
  (list (cons "statement" (statement q))
        (cons "parameters"
              (list (cons "props" (properties q))))))

(defmethod encode-cypher-query ((q cypher-query))
  (json:encode-json-alist-to-string (structure-cypher-query q)))

(defun make-query (statement &key properties)
  (let ((q (make-instance 'cypher-query
                          :statement statement
                          :properties properties)))
    q))
