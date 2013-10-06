#lang racket

(provide (struct-out fi-heap) (struct-out node) check-parents! add-node-to-dll! remove-node-from-dll! combine!)

(struct fi-heap (minref size) #:mutable)
(struct node (val parent children left right marked) #:mutable)

;; Corrects the ith rank slot of a vector such that it only has one root element
(define (combine! h node1 node2)
  (cond ((<= (node-val node1) (node-val node2))
         (set-node-parent! node2 node1)
         (set-node-children! node1 (vector-append (node-children node1) (vector node2)))
         (remove-node-from-dll! h node2)
         node1)
        ((> (node-val node1) (node-val node2))
         (set-node-parent! node1 node2)
         (set-node-children! node2 (vector-append (node-children node2) (vector node1)))
         (remove-node-from-dll! h node1)
         node2)))

;; Recursively checks if parent node is marked or not and adds parent to root vector of the heap if needed.
(define (check-parents! h parent child)
  (cond ((eq? parent #f) (void))
        ((not (node-marked parent))
         (set-node-children! parent (remove-node (node-children parent) child))
         (when (not (eq? #f (node-parent parent))) (set-node-marked! parent #t)))
        ((node-marked parent)
         (set-node-children! parent (remove-node (node-children parent) child))
         (set-node-marked! parent #f)
         (add-node-to-dll! h parent)
         (check-parents! h (node-parent parent) parent))))

;; Not deleting the node from the heap, but adding as a child of a root node
(define (remove-node-from-dll! h noderef)
  (let* ((nodeleft (node-left noderef))
         (noderight (node-right noderef)))
    (set-node-right! nodeleft noderight)
    (set-node-left! noderight nodeleft)
    (set-node-left! noderef noderef)
    (set-node-right! noderef noderef)))

(define (add-node-to-dll! h noderef)
  (let* ((minref (fi-heap-minref h))
         (minrightref (node-right minref)))
    (set-node-left! noderef minref)
    (set-node-right! noderef minrightref)
    (set-node-right! minref noderef)
    (set-node-left! minrightref noderef)))

;; Remove a node from a vector of nodes
(define (remove-node nodevec noderef)
  (for/fold ([res #()])
            ([n (in-vector nodevec)])
            (if (eq? n noderef) res
              (vector-append res (vector n)))))


