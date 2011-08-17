(in-package #:cl-neo4j)

(defun format-neo4j-query (host port resource &key (db-postfix "db/data/"))
  (format nil "http://~A:~A/~A~A" host port db-postfix resource))

(defgeneric encode-neo4j-json-payload (object encode-type &key)
  (:method (object encode-type &key)
    (declare (ignore encode-type))
    (encode-json-to-string object)))

(defmethod encode-neo4j-json-payload (object (encode-type (eql :node-url)) &key (host *neo4j-host*) (port *neo4j-port*))
  (declare (ignore encode-type))
  (format-neo4j-query host port (format nil "node/~A" object)))

(defmethod encode-neo4j-json-payload (object (encode-type (eql :node-url-single)) &key (host *neo4j-host*) (port *neo4j-port*))
  (declare (ignore encode-type))
  (encode-neo4j-json-payload (encode-neo4j-json-payload object :node-url :host host :port port) :string))

(defmethod encode-neo4j-json-payload (object (encode-type (eql :relationship-url)) &key (host *neo4j-host*) (port *neo4j-port*))
  (declare (ignore encode-type))
  (format-neo4j-query host port (format nil "relationship/~A" object)))

(defmethod encode-neo4j-json-payload (object (encode-type (eql :relationship-url-single)) &key (host *neo4j-host*) (port *neo4j-port*))
  (declare (ignore encode-type))
  (encode-neo4j-json-payload (encode-neo4j-json-payload object :relationship-url :host host :port port) :string))

(defmethod encode-neo4j-json-payload (object (encode-type (eql :object)) &key)
  (declare (ignore encode-type))
  (encode-neo4j-json-payload (mapcar (lambda (el)
                                       (cons (caar el) (if (cadr el)
                                                           (encode-neo4j-json-payload (cdar el)
                                                                                      (cadr el))
                                                           (cdar el))))
                                     object)
                             :string))

(defmethod encode-neo4j-json-payload (object (encode-type (eql :relationship)) &key)
  (declare (ignore encode-type))
  (destructuring-bind (to type data) object
    (encode-neo4j-json-payload (list (list (cons "to" to) :node-url)
                                     (list (cons "type" type))
                                     (list (cons "data" data)))
                               :object)))

(defun decode-neo4j-json-output (json)
  (decode-json-from-string (babel:octets-to-string json)))

(defun urlencode (string)
  (cl-ppcre:regex-replace "\\+" (drakma::url-encode string :latin1) "%20"))