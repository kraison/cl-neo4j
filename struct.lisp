(in-package #:cl-neo4j)

(defun extract-node-id (url)
  "http://localhost:9999/node/1"
  (subseq url (1+ (search "/" url :from-end t))))

(defun print-neo-relationship (r s d)
  (declare (ignore d))
  (format s "#<NEO-RELATIONSHIP ~A>" (neo-relationship-self-url r)))

(defstruct (neo-relationship
	     (:print-function print-neo-relationship)
	     (:predicate neo-relationship?))
  id self-url)

(defun translate-to-neo-relationship-struct (relationship-as-list)
  )

(defun make-neo-relationship-from-octets (octets)
  (let ((relationship (decode-json-from-string (map 'string #'code-char octets))))
    (if *use-structs*
	(translate-to-neo-relationship-struct relationship)
	relationship)))

(defun print-neo-node (n s d)
  (declare (ignore d))
  (format s "#<NEO-NODE-~A ~A>" (neo-node-id n) (neo-node-self-url n)))

(defstruct (neo-node
	     (:print-function print-neo-node)
	     (:predicate neo-node?))
  id self-url incoming-relationships-url outgoing-relationship-url properties-hash)

(defun translate-to-neo-node-struct (node-as-list)
  (let ((node
	 (make-neo-node :id (extract-node-id (cdr (assoc :self node-as-list)))
			:self-url (cdr (assoc :self node-as-list))
			:incoming-relationships-url 
			(cdr (assoc :|INCOMING RELATIONSHIPS| node-as-list))
			:outgoing-relationship-url 
			(cdr (assoc :|OUTGOING RELATIONSHIPS| node-as-list))
			:properties-hash (make-hash-table))))
    (dolist (prop (cdr (assoc :data node-as-list)))
      (setf (gethash (car prop) (neo-node-properties-hash node)) (cdr prop)))
    node))
	  
(defun make-neo-node-from-octets (octets)
  (let ((node (decode-json-from-string (map 'string #'code-char octets))))
    (if *use-structs*
	(translate-to-neo-node-struct node)
	node)))

(defmethod node-property ((node neo-node) key)
  (gethash key (neo-node-properties-hash node)))

