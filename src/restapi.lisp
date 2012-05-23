;;; Low-level REST api for the database. Returns raw unserialized responses.

(in-package #:cl-neo4j)

(def-neo4j-fun get-node (node-id)
  :get
  (:uri-spec (if node-id
                 (format nil "node/~A" node-id)
                 ""))
  (:status-handlers
   (200 (decode-neo4j-json-output body))
   (404 (error 'node-not-found-error :uri uri))))

(def-neo4j-fun create-node (properties)
  :post
  (:uri-spec (format nil "node"))
  (:encode properties :string)
  (:status-handlers
   (201 (decode-neo4j-json-output body))
   (400 (error 'invalid-data-sent-error :uri uri :json json))))

(def-neo4j-fun delete-node (node-id)
  :delete
  (:uri-spec (format nil "node/~A" node-id))
  (:status-handlers
   (204 (values t body))
   (404 (error 'node-not-found-error :uri uri))
   (409 (error 'unable-to-delete-node-error :uri uri))))

(def-neo4j-fun set-node-properties (node-id properties)
  :put
  (:encode properties :string)
  (:uri-spec (format nil "node/~A/properties" node-id))
  (:status-handlers
   (204 (values t body))
   (400 (error 'invalid-data-sent-error :uri uri :json json))
   (404 (error 'node-not-found-error :uri uri))))

(def-neo4j-fun get-node-properties (node-id)
  :get
  (:uri-spec (format nil "node/~A/properties" node-id))
  (:status-handlers
   (200 (decode-neo4j-json-output body))
   (204 nil)
   (404 (error 'node-not-found-error :uri uri))))

(def-neo4j-fun del-node-properties (node-id)
  :delete
  (:uri-spec (format nil "node/~A/properties" node-id))
  (:status-handlers
   (204 (values t body))
   (404 (error 'node-not-found-error :uri uri))))

(def-neo4j-fun set-node-property (node-id property value)
  :put
  (:uri-spec (format nil "node/~A/properties/~A" node-id
                     (if (symbolp property)
                         (string-downcase (symbol-name property))
                         property)))
  (:encode value :string)
  (:status-handlers
   (204 (values t body))
   (400 (error 'invalid-data-sent-error :uri uri :json json))))

(def-neo4j-fun get-node-property (node-id property)
  :get
  (:uri-spec (format nil "node/~A/properties/~A" node-id
                     (if (symbolp property)
                         (string-downcase (symbol-name property))
                         property)))
  (:status-handlers
   (200 (decode-neo4j-json-output body))
   (400 (error 'invalid-data-sent-error :uri uri :json json))
   (404 (error 'property-not-found-error :uri uri))))

(def-neo4j-fun del-node-property (node-id property)
  :delete
  (:uri-spec (format nil "node/~A/properties/~A" node-id
                     (if (symbolp property)
                         (string-downcase (symbol-name property))
                         property)))
  (:status-handlers
   (204 (values t body))
   (404 (error 'node-not-found-error :uri uri))
   (409 (error 'unable-to-delete-node-error :uri uri))))

(def-neo4j-fun get-relationship (relationship-id)
  :get
  (:uri-spec (format nil "relationship/~A" relationship-id))
  (:status-handlers
   (200 (decode-neo4j-json-output body))
   (404 (error 'relationship-not-found-error :uri uri))))

(def-neo4j-fun create-relationship (node-id to-node-id relationship-type properties)
  :post
  (:uri-spec (format nil "node/~A/relationships" node-id))
  (:encode (list to-node-id relationship-type properties) :relationship)
  (:status-handlers
   (201 (decode-neo4j-json-output body))
   (400 (error 'invalid-data-sent-error :uri uri :json json))
   (404 (error 'node-not-found-error :uri uri))))

(def-neo4j-fun delete-relationship (relationship-id)
  :delete
  (:uri-spec (format nil "relationship/~A" relationship-id))
  (:status-handlers
   (204 (values t body))
   (404 (error 'relationship-not-found-error :uri uri))))

(def-neo4j-fun set-relationship-properties (relationship-id properties)
  :put
  (:uri-spec (format nil "relationship/~A/properties" relationship-id))
  (:encode properties :string)
  (:status-handlers
   (204 (values t body))
   (400 (error 'invalid-data-sent-error :uri uri :json json))
   (404 (error 'relationship-not-found-error :uri uri))))

(def-neo4j-fun get-relationship-properties (relationship-id)
  :get
  (:uri-spec (format nil "relationship/~A/properties" relationship-id))
  (:status-handlers
   (200 (decode-neo4j-json-output body))
   (204 nil)
   (404 (error 'relationship-not-found-error :uri uri))))

(def-neo4j-fun del-relationship-properties (relationship-id)
  :delete
  (:uri-spec (format nil "relationship/~A/properties" relationship-id))
  (:status-handlers
   (204 (values t body))
   (404 (error 'relationship-not-found-error :uri uri))))

(def-neo4j-fun set-relationship-property (relationship-id property value)
  :put
  (:uri-spec (format nil "relationship/~A/properties/~A"
                     relationship-id
                     (if (symbolp property)
                         (string-downcase (symbol-name property))
                         property)))
  (:encode value :string)
  (:status-handlers
   (204 (values t body))
   (400 (error 'invalid-data-sent-error :uri uri :json json))
   (404 (error 'relationship-not-found-error :uri uri))))

(def-neo4j-fun get-relationship-property (relationship-id property)
  :get
  (:uri-spec (format nil "relationship/~A/properties/~A"
                     relationship-id
                     (if (symbolp property)
                         (string-downcase (symbol-name property))
                         property)))
  (:status-handlers
   (200 (decode-neo4j-json-output body))
   (400 (error 'invalid-data-sent-error :uri uri :json json))
   (404 (error 'property-not-found-error :uri uri))))

(def-neo4j-fun del-relationship-property (relationship-id property)
  :delete
  (:uri-spec (format nil "relationship/~A/properties/~A"
                     relationship-id
                     (if (symbolp property)
                         (string-downcase (symbol-name property))
                         property)))
  (:status-handlers
   (204 (values t body))
   (404 (error 'relationship-not-found-error :uri uri))))

(def-neo4j-fun get-node-relationships (node-id direction types)
  :get
  (:uri-spec (format nil "node/~A/relationships/~A/~{~A~^\\&~}"
                     node-id
                     (cond ((null direction) "all")
                           ((symbolp direction)
                            (string-downcase (symbol-name direction)))
                           (t direction))
                     types))
  (:status-handlers
   (200 (decode-neo4j-json-output body))
   (404 (error 'node-not-found-error :uri uri))))

(def-neo4j-fun get-relationships-types ()
  :get
  (:uri-spec "relationship/types")
  (:status-handlers
   (200 (decode-neo4j-json-output body))))

(def-neo4j-fun create-index ((type :node) name config)
  :post
  (:uri-spec (format nil "index/~A" (string-downcase (symbol-name type))))
  (:encode (list (cons "name" name) config) :string)
  (:status-handlers
   (201 (decode-neo4j-json-output body))))

(def-neo4j-fun delete-index ((type :node) name)
  :delete
  (:uri-spec (format nil "index/~A/~A" (string-downcase (symbol-name type)) name))
  (:status-handlers
   (204 (values t body))))

(def-neo4j-fun list-indexes ((type :node))
  :get
  (:uri-spec (format nil "index/~A" (string-downcase (symbol-name type))))
  (:status-handlers
   (200 (decode-neo4j-json-output body))))

(def-neo4j-fun add-to-index ((type :node) name key value object-id)
  :post
  (:uri-spec (format nil "index/~A/~A/~A/~A" (string-downcase (symbol-name type))
                     name key (urlencode value)))
  (:encode object-id (case type
                       (:node :node-url-single)
                       (:relationship :relationship-url-single)))
  (:status-handlers
   (201 (decode-neo4j-json-output body))))

(def-neo4j-fun remove-from-index ((type :node) name key value object-id)
  :delete
  (:uri-spec (format nil "index/~A/~A/~@[~A/~]~@[~A/~]~A" (string-downcase (symbol-name type))
                     name key (urlencode value) object-id))
  (:status-handlers
   (204 (values t body))
   (404 (error 'index-entry-not-found-error :uri uri))))

(def-neo4j-fun lookup-index ((type :node) name key value)
  :get
  (:uri-spec (format nil "index/~A/~A/~A/~A" (string-downcase (symbol-name type))
                     name key (urlencode value)))
  (:status-handlers
   (200 (decode-neo4j-json-output body))
   (404 (error 'index-entry-not-found-error :uri uri))))

(def-neo4j-fun query-index ((type :node) name query)
  :get
  (:uri-spec (format nil "index/~A/~A/?query=~A" (string-downcase (symbol-name type))
                     name (urlencode query)))
  (:status-handlers
   (200 (decode-neo4j-json-output body))
   (404 (error 'index-entry-not-found-error :uri uri))))

(def-neo4j-fun traverse (node-id (return-type :node) (max-depth 1) (order :depth-first)
                         uniqueness relationships prune-evaluator return-filter)
  :post
  (:uri-spec (format nil "node/~A/traverse/~A" node-id (string-downcase (symbol-name return-type))))
  (:encode (list
            (cons "order"
                  (case order
                    (:depth-first "depth_first")
                    (:breadth-first "breadth_first")))
            (cons "uniqueness" uniqueness)
            (cons "relationships" relationships)
            (cons "prune_evaluator" prune-evaluator)
            (cons "return_filter" return-filter)
            (cons "max_depth" max-depth))
           :string)
  (:status-handlers
   (200 (decode-neo4j-json-output body))
   (404 (error 'node-not-found-error :uri uri))))

(def-neo4j-fun get-path (node-id to-node-id relationships (max-depth 3) (algorithm :shortest-path))
  :post
  (:uri-spec (format nil "node/~A/path" node-id))
  (:encode (list (list (cons "to" to-node-id) :node-url)
                 (list (cons "relationships" relationships))
                 (list (cons "max_depth" max-depth))
                 (list (cons "algorithm" (case algorithm
                                           (:shortest-path "shortestPath")
                                           (:all-paths "allPaths")
                                           (:all-simple-paths "allSimplePaths")
                                           (:dijkstra "dijkstra")))))
           :object)
  (:status-handlers
   (200 (decode-neo4j-json-output body))
   (404 (error 'path-not-found-error :uri uri))))

(def-neo4j-fun get-paths (node-id to-node-id relationships (max-depth 3) (algorithm :shortest-path))
  :post
  (:uri-spec (format nil "node/~A/paths" node-id))
  (:encode (list (list (cons "to" to-node-id) :node-url)
                 (list (cons "relationships" relationships))
                 (list (cons "max_depth" max-depth))
                 (list (cons "algorithm" (case algorithm
                                           (:shortest-path "shortestPath")
                                           (:all-paths "allPaths")
                                           (:all-simple-paths "allSimplePaths")
                                           (:dijkstra "dijkstra")))))
           :object)
  (:status-handlers
   (200 (decode-neo4j-json-output body))
   (204 (error 'path-not-found-error :uri uri))))
