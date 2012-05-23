(in-package :cl-neo4j.tests)

(def-suite restapi :in cl-neo4j)

(in-suite restapi)

(test create-delete-node
  (let ((node-id (get-id-from-data (cl-neo4j:create-node))))
    (is (numberp node-id))
    (is (equal node-id
               (get-id-from-data (cl-neo4j:get-node :node-id node-id))))
    (is (cl-neo4j:delete-node :node-id node-id))
    (signals (cl-neo4j:node-not-found-error)
      (cl-neo4j:delete-node :node-id node-id))))

(test get-node
  (with-test-nodes (a b) ()
    (is (equal (get-id-from-data (cl-neo4j:get-node :node-id a))
               a))
    (cl-neo4j:delete-node :node-id b)
    (signals (cl-neo4j:node-not-found-error)
      (cl-neo4j:get-node :node-id b))))

(test node-properties
  (with-test-nodes (a) ()
    (is (cl-neo4j:set-node-properties :node-id a
                                      :properties '((:test . "test"))))
    (is (equal (cl-neo4j:get-node-properties :node-id a)
               '((:test . "test"))))
    (is (equal (cl-neo4j:get-node-property :node-id a :property :test)
               "test"))
    (is (cl-neo4j:del-node-properties :node-id a))
    (is (null (cl-neo4j:get-node-properties :node-id a)))
    (signals (cl-neo4j:property-not-found-error)
      (cl-neo4j:get-node-property :node-id a :property :test))
    (is (cl-neo4j:set-node-property :node-id a
                                    :property :test
                                    :value "test2"))
    (is (equal (cl-neo4j:get-node-properties :node-id a)
               '((:test . "test2"))))))

(test create-delete-relationship
  (with-test-nodes (a b) ()
    (let ((r (get-id-from-data
              (cl-neo4j:create-relationship :node-id a
                                            :to-node-id b
                                            :relationship-type "test"))))
      (is (get-id-from-data (cl-neo4j:get-relationship :relationship-id r))
          r)
      (is (cdr (assoc :type (cl-neo4j:get-relationship :relationship-id r)))
          "test")
      (is (cl-neo4j:delete-relationship :relationship-id r))
      (signals (cl-neo4j:relationship-not-found-error)
        (cl-neo4j:get-relationship :relationship-id r)))))

(test get-relationship
  (with-test-nodes (a b) ((ab a b "test") (ba b a "test2"))
    (is (equal (get-id-from-data (cl-neo4j:get-relationship
                                  :relationship-id ab))
               ab))
    (cl-neo4j:delete-relationship :relationship-id ba)
    (signals (cl-neo4j:relationship-not-found-error)
      (cl-neo4j:get-relationship :relationship-id ba))))

(test relationship-properties
  (with-test-nodes (a b) ((ab a b "test"))
    (is (cl-neo4j:set-relationship-properties :relationship-id ab
                                              :properties '((:test . "test"))))
    (is (equal (cl-neo4j:get-relationship-properties :relationship-id ab)
               '((:test . "test"))))
    (is (equal (cl-neo4j:get-relationship-property :relationship-id ab :property :test)
               "test"))
    (is (cl-neo4j:del-relationship-properties :relationship-id ab))
    (is (null (cl-neo4j:get-relationship-properties :relationship-id ab)))
    (signals (cl-neo4j:property-not-found-error)
      (cl-neo4j:get-relationship-property :relationship-id ab :property :test))
    (is (cl-neo4j:set-relationship-property :relationship-id ab
                                    :property :test
                                    :value "test2"))
    (is (equal (cl-neo4j:get-relationship-properties :relationship-id ab)
               '((:test . "test2"))))))

(test node-relationships
  (with-test-nodes (a b) ((ab1 a b "test")
                          (ab2 a b "test2")
                          (ab3 a b "test3"))
    (is (null
         (set-difference
          (mapcar #'get-id-from-data
                  (cl-neo4j:get-node-relationships :node-id a))
          (list ab1 ab2 ab3))))))

(test relationship-types
  (with-test-nodes (a b) ((ab1 a b "test")
                          (ab2 a b "test2")
                          (ab3 a b "test3"))
    (is (null
         (set-difference (cl-neo4j:get-relationships-types)
                         (list "test" "test2" "test3")
                         :test #'equal)))))

(test indexes
  (with-test-nodes (a b) ()
    (is (cl-neo4j:create-index :name "i1"))
    (is (cl-neo4j:delete-index :name "i1"))
    (signals (cl-neo4j:index-not-found-error)
      (cl-neo4j:delete-index :name "i1"))
    (is (cl-neo4j:create-index :name "i1"))
    (is (cl-neo4j:add-to-index :name "i1"
                               :key "k1"
                               :value "v1"
                               :object-id a))
    (is (cl-neo4j:add-to-index :name "i1"
                               :key "k1"
                               :value "v1"
                               :object-id b))
    (is (null
         (set-difference
          (mapcar #'get-id-from-data
                  (cl-neo4j:lookup-index :name "i1"
                                         :key "k1"
                                         :value "v1"))
          (list a b))))
    (is (cl-neo4j:remove-from-index :name "i1"
                                    :key "k1"
                                    :value "v1"
                                    :object-id b))
    (is (null
         (set-difference
          (mapcar #'get-id-from-data
                  (cl-neo4j:lookup-index :name "i1"
                                         :key "k1"
                                         :value "v1"))
          (list a))))
    (is (cl-neo4j:delete-index :name "i1"))))

(test traversal-and-paths
  (with-test-nodes (a b c d e f) ((ab a b "type1")
                                  (ac a c "type1")
                                  (bd b d "type1")
                                  (be b e "type1")
                                  (cf c f "type1")
                                  (fe f e "type1"))
    (is (equal (mapcar #'get-id-from-data
                       (cl-neo4j:traverse :node-id a))
               (list b c)))
    (is (equal (mapcar #'cl-neo4j-wrapper::extract-id-from-link
                       (cdr
                        (assoc :nodes
                               (cl-neo4j:get-path :node-id a
                                                  :to-node-id e))))
               (list a b e)))
    (is (equal (mapcar (compose (curry #'mapcar #'cl-neo4j-wrapper::extract-id-from-link)
                                #'cdr
                                (curry #'assoc :nodes))
                       (cl-neo4j:get-paths :node-id a
                                           :to-node-id e
                                           :max-depth 10
                                           :algorithm :all-paths))
               (list (list a b e)
                     (list a c f e))))))