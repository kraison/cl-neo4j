(in-package #:cl-neo4j)

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

(defmacro def-neo4j-fun (name lambda-list method &rest args)
  `(progn
     (defun ,name ,(append '(&key (host *neo4j-host*) (port *neo4j-port*)) lambda-list)
       (let ((uri ,(cadr (assoc :uri-spec args)))
	     (json ,(cond ((cadr (assoc :encode-properties? args))
			   `(encode-json-to-string properties))
			  ((cadr (assoc :encode-value? args))
			   `(encode-json-to-string value))
			  ((cadr (assoc :encode-node-id? args))
			   `(format nil "http://~A:~A/node/~A" host port node-id))
			  ((cadr (assoc :encode-relationship? args))
			   `(encode-json-to-string
			     (list (cons "to" 
					 (format nil "http://~A:~A/node/~A" host port to-node-id))
				   (cons "type" relationship-type)
				   (cons "data" properties))))
			  (t nil))))
	 (multiple-value-bind (body status headers drakma-uri stream must-close reason)
	     (http-request uri
			   :method ,method
			   :content json
			   :content-type (if json "application/json" nil)
			   :accept "application/json")
	   (declare (ignore headers drakma-uri stream must-close reason))
	   (case status
	     ,@(mapcar 
		#'(lambda (s) s) 
		(append (cadr (assoc :status-handlers args))
			`((othwerwise 
			   (error 'unknown-return-type-error :uri uri :status status
				  :property (cond ((boundp 'properties) (symbol-value 'properties))
						  ((boundp 'value) (symbol-value 'value))
						  ((boundp 'node-id) (symbol-value 'node-id))
						  (t nil)))))))))))))

(def-neo4j-fun get-node (node-id)
  :get
  (:uri-spec (if node-id
		 (format nil "http://~A:~A/node/~A" host port node-id)
		 (format nil "http://~A:~A/" host port)))
  (:status-handlers 
   ((200 (decode-json-from-string (map 'string #'code-char body)))
    (404 (error 'node-not-found-error :uri uri :property node-id)))))

(def-neo4j-fun create-node (properties)
  :post
  (:encode-properties? t)
  (:uri-spec (format nil "http://~A:~A/node" host port))
  (:status-handlers
   ((201 (decode-json-from-string (map 'string #'code-char body)))
    (400 (error 'invalid-data-sent-error :uri uri :json json)))))

(def-neo4j-fun delete-node (node-id)
  :delete
  (:uri-spec (format nil "http://~A:~A/node/~A" host port node-id))
  (:status-handlers
   ((204 (values t body))
    (404 (error 'node-not-found-error :uri uri))
    (409 (error 'unable-to-delete-node-error :uri uri)))))

(def-neo4j-fun set-node-properties (node-id properties)
  :put
  (:encode-properties? t)
  (:uri-spec (format nil "http://~A:~A/node/~A/properties" host port node-id))
  (:status-handlers
   ((204 (values t body))
    (400 (error 'invalid-data-sent-error :uri uri :json json))
    (404 (error 'node-not-found-error :uri uri)))))

(def-neo4j-fun get-node-properties (node-id)
  :get
  (:uri-spec (format nil "http://~A:~A/node/~A/properties" host port node-id))
  (:status-handlers
   ((200 (decode-json-from-string (map 'string #'code-char body)))
    (204 nil)
    (404 (error 'node-not-found-error :uri uri)))))
   
(def-neo4j-fun del-node-properties (node-id)
  :delete
  (:uri-spec (format nil "http://~A:~A/node/~A/properties" host port node-id))
  (:status-handlers
   ((202 (values t body))
    (404 (error 'node-not-found-error :uri uri)))))

(def-neo4j-fun set-node-property (node-id property value)
  :put
  (:encode-value? t)
  (:uri-spec (format nil "http://~A:~A/node/~A/properties/~A" host port node-id 
		     (if (symbolp property)
			 (string-downcase (symbol-name property))
			 property)))
  (:status-handlers
   ((204 (values t body))
    (400 (error 'invalid-data-sent-error :uri uri :json json)))))

(def-neo4j-fun get-node-property (node-id property)
  :get
  (:uri-spec (format nil "http://~A:~A/node/~A/properties/~A" host port node-id
		     (if (symbolp property)
			 (string-downcase (symbol-name property))
			 property)))
  (:status-handlers
   ((200 (decode-json-from-string (map 'string #'code-char body)))
    (400 (error 'invalid-data-sent-error :uri uri :json json)))))

(def-neo4j-fun del-node-property (node-id property)
  :delete
  (:uri-spec (format nil "http://~A:~A/node/~A/properties/~A" host port node-id
		     (if (symbolp property)
			 (string-downcase (symbol-name property))
			 property)))
  (:status-handlers
   ((204 (values t body))
    (404 (error 'node-not-found-error :uri uri))
    (409 (error 'unable-to-delete-node-error :uri uri)))))

(def-neo4j-fun create-relationship (node-id to-node-id relationship-type properties)
  :post
  (:encode-relationship? t)
  (:uri-spec (format nil "http://~A:~A/node/~A/relationships" host port node-id))
  (:status-handlers
   ((201 (decode-json-from-string (map 'string #'code-char body)))
    (400 (error 'invalid-data-sent-error :uri uri :json json))
    (404 (error 'node-not-found-error :uri to-node-id)))))

(def-neo4j-fun set-relationship-properties (relationship-id properties)
  :put
  (:encode-properties? t)
  (:uri-spec (format nil "http://~A:~A/relationship/~A/properties" host port relationship-id))
  (:status-handlers
   ((204 (values t body))
    (400 (error 'invalid-data-sent-error :uri uri :json json))
    (404 (error 'relationship-not-found-error :uri uri)))))

(def-neo4j-fun get-relationship-properties (relationship-id)
  :get
  (:uri-spec (format nil "http://~A:~A/relationship/~A/properties" host port relationship-id))
  (:status-handlers
   ((200 (decode-json-from-string (map 'string #'code-char body)))
    (204 nil)
    (404 (error 'relationship-not-found-error :uri uri)))))
   
(def-neo4j-fun del-relationship-properties (relationship-id)
  :delete
  (:uri-spec (format nil "http://~A:~A/relationship/~A/properties" host port relationship-id))
  (:status-handlers
   ((202 (values t body))
    (404 (error 'relationship-not-found-error :uri uri)))))

(def-neo4j-fun set-relationship-property (relationship-id property value)
  :put
  (:encode-value? t)
  (:uri-spec (format nil "http://~A:~A/relationship/~A/properties/~A" 
		     host port relationship-id property))
  (:status-handlers
   ((204 (values t body))
    (400 (error 'invalid-data-sent-error :uri uri :json json))
    (404 (error 'relationship-not-found-error :uri uri)))))

(def-neo4j-fun get-relationship-property (relationship-id property)
  :get
  (:uri-spec (format nil "http://~A:~A/relationship/~A/properties/~A" 
		     host port relationship-id property))
  (:status-handlers
   ((200 (decode-json-from-string (map 'string #'code-char body)))
    (400 (error 'invalid-data-sent-error :uri uri :json json)))))

(def-neo4j-fun del-relationship-property (relationship-id property)
  :delete
  (:uri-spec (format nil "http://~A:~A/relationship/~A/properties/~A" 
		     host port relationship-id property))
  (:status-handlers
   ((204 (values t body))
    (404 (error 'relationship-not-found-error :uri uri)))))

(def-neo4j-fun delete-relationship (relationship-id)
  :delete
  (:uri-spec (format nil "http://~A:~A/relationship/~A" host port relationship-id))
  (:status-handlers
   ((204 (values t body))
    (404 (error 'relationship-not-found-error :uri uri)))))

(def-neo4j-fun get-node-relationships (node-id direction types)
  :get
  (:uri-spec (format nil "http://~A:~A/node/~A/relationships/~A/~{~A~^\\&~}" 
		     host port node-id 
		     (cond ((null direction) "all")
			   ((symbolp direction)
			    (string-downcase (symbol-name direction)))
			   (t direction)) 
		     types))
  (:status-handlers
   ((200 (decode-json-from-string (map 'string #'code-char body)))
    (404 (error 'node-not-found-error :uri uri :property node-id)))))

(def-neo4j-fun list-indices ()
  :get
  (:uri-spec (format nil "http://~A:~A/index" host port))
  (:status-handlers
   ((200 (decode-json-from-string (map 'string #'code-char body))))))

(def-neo4j-fun add-to-index (node-id key value)
  :post
  (:uri-spec (format nil "http://~A:~A/index/node/~A/~A" host port key value))
  (:encode-node-id? t)
  (:status-handlers
   ((201 (values t body)))))

(def-neo4j-fun remove-from-index (node-id key value)
  :delete
  (:uri-spec (format nil "http://~A:~A/index/node/~A/~A/~A" host port key value node-id))
  (:status-handlers
   ((204 (values t body))
    (404 (error 'index-entry-not-found-error :uri uri)))))

(def-neo4j-fun query-index (key value)
  :get
  (:uri-spec (format nil "http://~A:~A/index/node/~A/~A" host port key value))
  (:status-handlers
   ((200 (decode-json-from-string (map 'string #'code-char body))))))

(def-neo4j-fun query-fulltext-index (key value)
  :get
  (:uri-spec (format nil "http://~A:~A/index/node-fulltext/~A/~A" host port key value))
  (:status-handlers
   ((200 (decode-json-from-string (map 'string #'code-char body))))))

(defun traverse (&key (host *neo4j-host*) (port *neo4j-port*) node-id (return-type :node) 
		 (max-depth 1) (order :depth-first) uniqueness relationships prune-evaluator 
		 return-filter)
  (let ((uri (format nil "http://~A:~A/node/~A/traverse/~A" 
		     host port node-id (string-downcase (symbol-name return-type))))
	(json (with-output-to-string (s)
		(format s "{\"order\":\"~A\"," (case order
						 (:depth-first "depth first")
						 (:breadth-first "breadth first")))
		(if uniqueness (format s "\"uniqueness\":\"~A\"," uniqueness))
		(if relationships (format s "\"relationships\":~A,"
					  (encode-json-to-string relationships)))
		(if prune-evaluator (format s "\"prune evaluator\":~A,"
					    (encode-json-to-string prune-evaluator)))
		(if return-filter (format s "\"return filter\":~A,"
					  (encode-json-to-string return-filter)))
		(format s "\"max depth\":~A}" max-depth))))
    (multiple-value-bind (status body) (do-neo4j-query uri :post :json json)
      (case status
	(200 (decode-json-from-string (map 'string #'code-char body)))
	(404 (error 'node-not-found-error :uri uri))
	(otherwise 
	 (error 'unknown-return-type-error :status status :uri uri :property nil))))))
	 
