#lang racket

(provide (struct-out fi-heap) (struct-out node) check-parents! add-node-to-dll! remove-node-from-dll! combine!)

(struct fi-heap (minref size) #:mutable)
(struct node (val parent children left right marked) #:mutable)

;; Corrects the ith rank slot of a vector such that it only has one root element
(define (combine! h existing current)

  (define (make-child! parentnode childnode)
    (set-node-parent! childnode parentnode)
    (set-node-children! parentnode (vector-append (node-children parentnode) (vector childnode)))
    parentnode)

  (cond ((< (node-val existing) (node-val current))
         (replace-in-dll! existing current)
         (make-child! existing current))
        ((> (node-val existing) (node-val current))
         (remove-node-from-dll! existing)
         (make-child! current existing))
        ((= (node-val existing) (node-val current))
         (cond ((eq? existing (fi-heap-minref h))
                (replace-in-dll! existing current)
                (make-child! existing current))
               (else
                 (remove-node-from-dll! existing)
                 (make-child! current existing))))))

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
(define (remove-node-from-dll! noderef)
  (let* ((nodeleft (node-left noderef))
         (noderight (node-right noderef)))
    (set-node-right! nodeleft noderight)
    (set-node-left! noderight nodeleft)
    (set-node-left! noderef noderef)
    (set-node-right! noderef noderef)))

; replaces node2 with node1 in dll
(define (replace-in-dll! node1 node2)
  (remove-node-from-dll! node1)
  (cond ((and (eq? node2 (node-left node2)) (eq? node2 (node-left node2)))
         (set-node-left! node2 node2)
         (set-node-right! node2 node2))
        (else
          (let ((rnode (node-right node2))
                (lnode (node-left node2)))
            (set-node-left! node1 lnode)
            (set-node-right! node1 rnode)
            (set-node-right! lnode node1)
            (set-node-left! rnode node1)
            (set-node-left! node2 node2)
            (set-node-right! node2 node2)))))

(define (add-node-to-dll! h noderef)
  (cond ((= (fi-heap-size h) 0) (raise-argument-error 'add-node-to-dll! "heap size 0" 0 h noderef))
        ((not (node? noderef)) (raise-argument-error 'add-node-to-dll! "node?" 1 h noderef))
        (else
          (let* ((minref (fi-heap-minref h))
                 (minrightref (node-right minref)))
            (set-node-left! noderef minref)
            (set-node-right! noderef minrightref)
            (set-node-right! minref noderef)
            (set-node-left! minrightref noderef)))))

;; Remove a node from a vector of nodes
(define (remove-node nodevec noderef)
  (for/fold ([res #()])
            ([n (in-vector nodevec)])
            (if (eq? n noderef) res
              (vector-append res (vector n)))))


