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

(defmethod encode-neo4j-json-payload ((object cypher-query) (encode-type (eql :statement)) &key)
  (declare (ignore encode-type))
  (encode-cypher-query object))

(defmethod encode-neo4j-json-payload (object (encode-type (eql :statements)) &key)
  (declare (ignore encode-type))
  (let ((table (make-hash-table :test 'equalp)))
    (setf (gethash "statements" table)
          (mapcar (lambda (statement)
                    (structure-cypher-query statement))
                  object))
    (encode-neo4j-json-payload table :table)))
