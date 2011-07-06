;; Higher lever wrapper for neo4j REST. Users need to define at least node-id and relationship-id accessors for portability + constructors.

(in-package #:cl-neo4j-wrapper)

;;; Protocol

;; Those variables define default return type for factory methods

(defvar *default-node-constructor*
  'make-standard-node)

(defvar *default-relationship-constructor*
  'make-standard-relationship)

;; Nodes

(defun node-create (&key (constructor *default-node-constructor*) properties initial-connections initial-indexes)
  "Creates a new node in the graph. Returns created node.

   constructor is a constructor for nodes. By default it uses *default-node-constructor*.
   properties are plist of node properties.
   initial-connections are list of lists of node (or ids), type and direction that node should be connected to.
   initial-indexes are list of lists of index name, key and value of indexes that node should be added to."
  (let* ((node (funcall constructor (create-node :properties properties))))
    (mapc (curry #'apply
                   (lambda (target type direction)
                     (relationship-create node target type :direction direction)))
          initial-connections)
    (mapc (curry #'apply
                 (lambda (index key value)
                   (node-add-to-index node index key value)))
          initial-indexes)
    node))

(defun node-get-by-id (id &key (constructor *default-node-constructor*))
  "Returns a node with given id or nil otherwise. This is a factory method, it accepts keyword argument constructor whcih is defaulted to *default-node-constructor*"
  (funcall constructor (get-node :node-id id)))

(defgeneric node-delete (node &key)
  (:documentation "Deletes a node. Cascade deletes the node even if node had relationships (deleting them too).")
  (:method :before (node &key cascade)
    (when cascade
      (mapc #'relationship-delete (node-relationships node))))
  (:method ((node integer) &key)
    (delete-node :node-id node)))

(defgeneric node-properties (node)
  (:documentation "Returns plist of properties of the node.")
  (:method ((node integer))
    (get-node-properties :node-id node)))

(defgeneric node-property (node property)
  (:documentation "Returns value of the property of the node. Returns nil if property is undefined.")
  (:method ((node integer) property)
    (get-node-property :node-id node :property property)))

(defgeneric (setf node-property) (value node property)
  (:documentation "Sets a value of the property of the node to value. Value of nil deletes property.")
  (:method (value (node integer) property)
    (if value
        (set-node-property :node-id node :property property :value value)
        (del-node-property :node-id node :property property))))

(defgeneric node-relationships (node &key types direction)
  (:documentation "Returns list of node relations, optionally filtered by list of possible types and direction. This is a factory method, accepts *default-node-constructor*")
  (:method :around (node &key (constructor *default-node-constructor*) &allow-other-keys)
    (mapcar constructor (call-next-method)))
  (:method ((node integer) &key types direction)
    (get-node-relationships :node-id node :types types :direction direction)))

(defgeneric node-add-to-index (node index key value)
  (:documentation "Adds node to index with key and value.")
  (:method ((node integer) index key value)
    (add-to-index :type :node
                  :name index
                  :key key
                  :value value
                  :object-id node)))

(defgeneric node-remove-from-index (node index &optional key value)
  (:documentation "Removes node from index. Optionally removes only from index with key and value.")
  (:method ((node integer) index &optional key value)
    (remove-from-index :type :node
                       :name index
                       :key key
                       :value value
                       :object-id node)))

(defun node-query-index (index key value &key (constructor *default-node-constructor*))
  "Returns list of nodes in the index with key and value.

  This is a factory method, it accepts keyword argument constructor which is defaulted to *default-node-constructor*"
  (mapcar constructor (lookup-index :type :node
                                    :name index
                                    :key key
                                    :value value)))

;; Relationships

(defgeneric relationship-create (node1 node2 type &key properties direction initial-indexes)
  (:documentation
   "Creates new relationship in a graph. Returns created relationship. If any step fails it tries to cleanup after itself.

    constructor is a constructor for relationships. By default it uses *default-relationship-constructor*.
    properties are plist of relationship properties.
    direction indicates a direction of relation from the point of view of node1.
    initial-indexes are list of lists of index name, key and value of indexes that node should be added to")
  (:method :around (node1 node2 type &key (constructor *default-relationship-constructor*) properties (direction :from) initial-indexes)
    (when (eq direction :all)
      (relationship-create node2 node1 type :properties properties :direction direction :initial-indexes initial-indexes))
    (funcall constructor (call-next-method node1 node2 type
                                           :properties properties
                                           :direction direction
                                           :initial-indexes initial-indexes)))
  (:method ((node1 integer) (node2 integer) type &key properties direction initial-indexes)
    (destructuring-bind (start end)
        (case direction
          (:from (list node1 node2))
          (:to (list node1 node2)))
      (let ((relationship (create-relationship :node-id start
                                               :to-node-id end
                                               :relationship-type type
                                               :properties properties)))
        (mapc (curry #'apply
                     (lambda (index key value)
                       (relationship-add-to-index relationship index key value)))
              initial-indexes)
        relationship))))

(defun relationship-get-by-id (id &key (constructor *default-node-constructor*))
  "Returns relationship with given id or nil otherwise."
  (funcall constructor (get-relationship :relationship-id id)))

(defgeneric relationship-delete (relationship)
  (:documentation "Deletes a relationship.")
  (:method ((relationship integer))
    (delete-relationship :relationship-id relationship)))

(defgeneric relationship-start (relationship)
  (:documentation "Returns start node of relationship.")
  (:method ((relationship integer))
    (node-get-by-id (extract-id-from-link
                     (cdr (assoc :start (get-relationship :relationship-id relationship)))))))

(defgeneric relationship-end (relationship)
  (:documentation "Returns end node of relationship.")
  (:method ((relationship integer))
    (node-get-by-id (extract-id-from-link
                     (cdr (assoc :end (get-relationship :relationship-id relationship)))))))

(defgeneric relationship-type (relationship)
  (:documentation "Returns type of the relationship.")
  (:method ((relationship integer))
    (cdr (assoc :type (get-relationship :relationship-id relationship)))))

(defgeneric relationship-properties (relationship)
  (:documentation "Returns plist of relationship properties.")
  (:method ((relationship integer))
    (get-relationship-properties :relationship-id relationship)))

(defgeneric relationship-property (relationship property)
  (:documentation "Returns value of the property of the relationship. Returns nil if property is undefined.")
  (:method ((relationship integer) property)
    (get-relationship-property :relationship-id relationship
                               :property property)))

(defgeneric (setf relationship-property) (value relationship property)
  (:documentation "Sets a value of the property of the relationship to value. Value of nil deletes property.")
  (:method (value (relationship integer) property)
    (if value
        (set-relationship-property :relationship-id relationship :property property :value value)
        (del-relationship-property :relationship-id relationship :property property))))

(defgeneric relationship-add-to-index (relationship index key value)
  (:documentation "Adds Relationship to index with key and value.")
  (:method ((relationship integer) index key value)
    (add-to-index :type :relationship
                  :name index
                  :key key
                  :value value
                  :object-id relationship)))

(defgeneric relationship-remove-from-index (relationship index &optional key value)
  (:documentation "Removes relationship from index. Optionally removes only from index with key and value.")
  (:method ((relationship integer) index &optional key value)
    (remove-from-index :type :relationship
                       :name index
                       :key key
                       :value value
                       :object-id relationship)))

(defun relationship-query-index (index key value &key (constructor *default-relationship-constructor*))
  "Returns list of nodes in the index with key and value."
  (mapcar constructor (lookup-index :type :relationship
                                    :name index
                                    :key key
                                    :value value)))

;;; Implementation

(defun extract-id-from-link (link)
  (parse-integer (car (split-sequence #\/ link :from-end t :count 1))))

(defun normalize-alist (alist)
  (mapcar (lambda (el)
            (cons (json:lisp-to-camel-case (symbol-name (car el)))
                  (cdr el)))
          alist))

(defclass standard-node ()
  ((id
    :initarg :id
    :accessor node-id)
   (properties
    :initarg :properties
    :accessor node-properties)))

(defmethod print-object ((object standard-node) stream)
  (print-unreadable-object (object stream :type t :identity '())
    (princ (node-id object) stream)))

(defun make-standard-node (alist)
  (make-instance 'standard-node
                 :id (extract-id-from-link (cdr (assoc :self alist)))
                 :properties (normalize-alist (cdr (assoc :data alist)))))

(defmethod node-delete ((node standard-node) &key)
  (node-delete (node-id node)))

(defmethod node-property ((node standard-node) property)
  (assoc property (node-properties node) :test #'equal))

(defmethod (setf node-property) (value (node standard-node) property)
  (setf (node-property (node-id node) property) value)
  (setf (node-properties node) (normalize-alist (get-node-properties :node-id (node-id node)))))

(defmethod node-relationships ((node standard-node) &key types direction)
  (node-relationships (node-id node) :types types :direction direction))

(defmethod node-add-to-index ((node standard-node) index key value)
  (node-add-to-index (node-id node) index key value))

(defmethod node-remove-from-index ((node standard-node) index &optional key value)
  (node-remove-from-index (node-id node) index key value))

(defclass standard-relationship ()
  ((id
    :initarg :id
    :accessor relationship-id)
   (properties
    :initarg :properties
    :accessor relationship-properties)
   (type
    :initarg :type
    :accessor relationship-type)
   (start-id
    :initarg :start
    :accessor %relationship-start-id)
   (end-id
    :initarg :end
    :accessor %relationship-end-id)))

(defmethod print-object ((object standard-relationship) stream)
  (print-unreadable-object (object stream :type t :identity '())
    (format stream "~A [~A--~A->~A]"
            (relationship-id object)
            (%relationship-start-id object)
            (relationship-type object)
            (%relationship-end-id object))))

(defun make-standard-relationship (alist)
  (make-instance 'standard-relationship
                 :id (extract-id-from-link (cdr (assoc :self alist)))
                 :properties (normalize-alist (cdr (assoc :data alist)))
                 :type (cdr (assoc :type alist))
                 :start (extract-id-from-link (cdr (assoc :start alist)))
                 :end (extract-id-from-link (cdr (assoc :end alist)))))

(defmethod relationship-delete ((relationship standard-relationship))
  (relationship-delete (relationship-id relationship)))

(defmethod relationship-start ((relationship standard-relationship))
  (node-get-by-id (%relationship-start-id relationship)))

(defmethod relationship-end ((relationship standard-relationship))
  (node-get-by-id (%relationship-end-id relationship)))

(defmethod relationship-property ((relationship standard-relationship) property)
  (assoc property (relationship-properties relationship) :test #'equal))

(defmethod (setf relationship-property) (value (relationship standard-relationship) property)
  (setf (relationship-property (relationship-id relationship) property) value)
  (setf (relationship-properties relationship)
        (normalize-alist (get-relationship-properties :relationship-id (relationship-id relationship)))))

(defmethod relationship-add-to-index ((relationship standard-relationship) index key value)
  (relationship-add-to-index (relationship-id relationship) index key value))

(defmethod relationship-remove-from-index ((relationship standard-relationship) index &optional key value)
  (relationship-remove-from-index (relationship-id relationship) index key value))