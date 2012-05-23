(in-package :cl-neo4j.tests)

(defmacro with-test-nodes (nodes relations &body body)
  (let ((node-bindings
          (mapcar (rcurry #'list
                          '(get-id-from-data (cl-neo4j:create-node)))
                  nodes))
        (relation-bindings
          (mapcar (lambda (rel)
                    (destructuring-bind (relation from to type)
                        rel
                      (list relation
                            `(get-id-from-data (cl-neo4j:create-relationship :node-id ,from :to-node-id ,to :relationship-type ,type)))))
                  relations)))
    `(let (,@node-bindings)
       (let (,@relation-bindings)
         (unwind-protect
              (progn ,@body)
           (mapc (lambda (r) (ignore-errors
                              (cl-neo4j:delete-relationship :relationship-id r)))
                 (list ,@(mapcar #'car relations)))
           (mapc (lambda (n) (ignore-errors
                              (cl-neo4j:delete-node :node-id n)))
                 (list ,@nodes)))))))

(defun get-id-from-data (data)
  (cl-neo4j-wrapper::extract-id-from-link
   (cdr (assoc :self data))))