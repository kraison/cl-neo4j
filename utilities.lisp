(in-package #:cl-neo4j)

(defmacro def-neo4j-fun (name lambda-list method &rest args)
  `(progn
     (defun ,name ,(append '(&key (host *neo4j-host*) (port *neo4j-port*)) lambda-list)
       (let ((uri (format-neo4j-query host port ,(cadr (assoc :uri-spec args))))
             (json (encode-neo4j-json-payload ,@(aif (assoc :encode args)
                                                     (cdr it)
                                                     (list '() :string)))))
         (multiple-value-bind (status body)
             (do-neo4j-query uri ,method :json json)
           (case status
             ,@(append (cdr (assoc :status-handlers args))
                       `((otherwise
                          (error 'unknown-return-type-error :uri uri :status status
                                 :property (list ,@(mapcar (lambda (arg) (if (consp arg) (car arg) arg))
                                                           lambda-list))))))))))))

(defun do-neo4j-query (uri method &key json decode?)
  (multiple-value-bind (body status headers uri stream must-close reason)
      (http-request uri
                    :method method
                    :content json
                    :content-type (if json "application/json")
                    :accept "application/json")
    (declare (ignore headers uri stream must-close reason))
    (values status (if (and decode? body)
                       (decode-json-from-string (map 'string #'code-char body))
                       body))))

(defun format-neo4j-query (host port resource &key (db-postfix "db/data/"))
  (format nil "http://~A:~A/~A~A" host port db-postfix resource))

(defgeneric encode-neo4j-json-payload (object encode-type &key)
  (:method (object encode-type &key)
    (declare (ignore encode-type))
    (encode-json-to-string object)))

(defmethod encode-neo4j-json-payload (object (encode-type (eql :node-url)) &key (host *neo4j-host*) (port *neo4j-port*))
  (declare (ignore encode-type))
  (format-neo4j-query host port (format nil "node/~A" object)))

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