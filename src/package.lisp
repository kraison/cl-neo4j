(in-package #:cl-user)

(defpackage #:cl-neo4j
  (:use #:cl
        #:alexandria
        #:anaphora
	#:json
	#:json-rpc
	#:drakma)
  (:export #:get-node
           #:create-node
           #:delete-node
	   #:set-node-properties
	   #:get-node-properties
	   #:del-node-properties
	   #:set-node-property
	   #:get-node-property
	   #:del-node-property
           #:get-relationship
           #:create-relationship
	   #:set-relationship-properties
	   #:get-relationship-properties
	   #:del-relationship-properties
	   #:set-relationship-property
	   #:get-relationship-property
	   #:del-relationship-property
	   #:delete-relationship
	   #:get-node-relationships
           #:get-relationships-types
	   #:create-index
           #:delete-index
           #:add-to-index
           #:remove-from-index
           #:lookup-index
           #:query-index
           #:traverse
           #:get-path
           #:get-paths
           ;; Conditions
           #:unknown-return-type-error
           #:invalid-data-sent-error
           #:node-not-found-error
           #:unable-to-delete-node-error
           #:relationship-not-found-error
           #:index-entry-not-found-error
           #:path-not-found-error
	   ;; Vars
	   #:*neo4j-host*
	   #:*neo4j-port*))

(defpackage #:cl-neo4j-wrapper
  (:use #:cl
        #:alexandria
        #:anaphora
        #:split-sequence
        #:cl-neo4j)
  (:export #:node-create
           #:node-get-by-id
           #:node-delete
           #:node-properties
           #:node-property
           #:node-relationships
           #:node-add-to-index
           #:node-remove-from-index
           #:node-query-index
           #:node-traverse

           #:relationship-create
           #:relationship-get-by-id
           #:relationship-delete
           #:relationship-start
           #:relationship-end
           #:relationship-type
           #:relationship-properties
           #:relationship-property
           #:relationship-add-to-index
           #:relationship-remove-from-index
           #:relationship-query-index
           #:relationship-traverse

           #:standard-node
           #:standard-relationship

           #:node-id
           #:relationship-id
           ;: Vars
           #:*default-node-constructor*
           #:*default-relationship-constructor*))
