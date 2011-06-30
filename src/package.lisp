(in-package #:cl-user)

(defpackage #:cl-neo4j
  (:use #:cl
        #:alexandria
        #:anaphora
	#:json
	#:json-rpc
	#:drakma)
  (:export #:do-neo4j-query
	   #:get-node
	   #:extract-node-id
	   #:create-node
	   #:set-node-properties
	   #:get-node-properties
	   #:del-node-properties
	   #:set-node-property
	   #:get-node-property
	   #:del-node-property
	   #:delete-node
	   #:create-relationship
	   #:set-relationship-properties
	   #:get-relationship-properties
	   #:del-relationship-properties
	   #:set-relationship-property
	   #:get-relationship-property
	   #:del-relationship-property
	   #:delete-relationship
	   #:get-node-relationships
	   #:list-indices
	   #:add-to-index
	   #:remove-from-index
	   #:query-index
	   #:traverse
	   ;; Vars
	   #:*neo4j-host*
	   #:*neo4j-port*
	   #:*use-structs*
	   ;; Node struct
	   #:neo-node?
	   #:neo-node-self-url
	   #:neo-node-id
	   ))
