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
	   ;; Vars
	   #:*neo4j-host*
	   #:*neo4j-port*))

(defpackage #:cl-neo4j-wrapper
  (:use #:cl
        #:alexandria
        #:anaphora
        #:split-sequence
        #:cl-neo4j))
