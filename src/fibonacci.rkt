#lang racket 
;; Core functions that implement Fibonacci heaps. Developed by Michael L. Fredman and Robert E. Tarjan in 1984. Core functions and amortized costs are as follows:
;; makeheap       - creates a Fibonacci heap with the number value provided. Amortized cost is O(1)
;; findmin        - returns the minimum value in the heap. Amortized cost is O(1)
;; insert!        - adds a number value to the heap. Amortized cost is O(1)
;; deletemin!     - removes the reference of the node with the min value. Amortized cost is O(log n)
;; meld           - melds two heaps into a new heap, Amortized cost is O(1)
;; decrement!     - decrements the value of the node by delta. Amortized cost is O(1)
;; delete!        - deletes a node from the heap. Amortized cost is O(log n)

;; Additional functions:
;; heap-minref    - gives the reference to min node in the heap 
;; heap-roots     - returns a vector with references to all roots in the heap (based on the rank)
;; heap-size      - gives the number of number values inserted in the heap
;; node-val       - gives the value stored in the node
;; node-children  - returns a vector of all child node references in the node
;; node-parent    - returns reference to the parent node

(require "fibonacci_helper.rkt")

(provide fi-makeheap fi-findmin fi-insert! fi-deletemin! fi-meld fi-decrement! fi-delete! fi-heap-minref fi-heap-roots fi-heap-size fi-node-val fi-node-children fi-node-parent)

;; Returns a new fibonacci heap containing only one number
(define (fi-makeheap val)
  (cond ((not (number? val)) (raise-argument-error 'fi-makeheap "number?" val))
        (else (let ((n (node val #f #() #f)))
               (fi-heap n (vector (vector n)) 1)))))

;; Returns the number in the min node of the heap
(define (fi-findmin h)
  (cond ((not (fi-heap? h)) (raise-argument-error 'fi-findmin "fi-heap?" h))
        ((= (fi-heap-size h) 0) (raise-type-error 'fi-findmin "node" (fi-heap-minref h)))
        (else (node-val (fi-heap-minref h)))))

;; Inserts a number to an existing heap
(define (fi-insert! h val)
  (cond ((not (fi-heap? h)) (raise-argument-error 'fi-insert! "fi-heap?" 0 h val))
        ((not (number? val)) (raise-argument-error 'fi-insert! "number?" 1 h val))
        (else (let ((n (node val #f #() #f)))
               (when (= 0 (fi-heap-size h)) (set-fi-heap-minref! h n))
               (vector-set! (fi-heap-roots h) 0 (vector-append (vector-ref (fi-heap-roots h) 0) (vector n)))
               (set-fi-heap-size! h (+ (fi-heap-size h) 1))
               (when (and (> (fi-heap-size h) 0) (< val (fi-findmin h))) (set-fi-heap-minref! h n))))))

;; Modifies the heap provided and its nodes
;; Commentary:
;; - this procedure mutates node->parent & node->children of the previously defined nodes in the heap
;; - since deletemin is the only time root vectors are properly sorted as per their rank, we create a vector of size (log n) such that all nodes 
;;   (after finding the unique w.r.t. rank) are put in the right slot of the rank vector
(define (fi-deletemin! h)
  (cond ((not (fi-heap? h)) (raise-argument-error 'fi-deletemin! "fi-heap?" h))
        ((= (fi-heap-size h) 0) (raise-type-error 'fi-deletemin! "node" (fi-heap-minref h)))
        (else 
          (let* ((maxrnk (+ 1 (inexact->exact (ceiling (/ (log (fi-heap-size h)) (log 2))))))

                 ; put children of min into a root vector
                 (newrts (create-children-rts-vec (node-children (fi-heap-minref h)) maxrnk))
                 (oldrts (fi-heap-roots h)))

            ; combine other roots into the ^new root vector 
            (for ([i (in-range (vector-length newrts))])
                 (let ((ithrankvec1 (vec-ref oldrts i))
                       (ithrankvec2 (vec-ref newrts i)))
                   (vector-set! newrts i (vector-append ithrankvec1 ithrankvec2))))
            
            ; remove min node from newrts
            (let ((minrank (vector-length (node-children (fi-heap-minref h))))
                  (minref (fi-heap-minref h)))
             (vector-set! newrts minrank 
                          (for/fold ([res #()])
                                    ([n (in-vector (vector-ref newrts minrank))])
                                      (if (eq? n minref) res 
                                        (vector-append res (vector n))))))

            ; certain no two roots of same rank in the rank vector
            (for ([i (in-range (vector-length newrts))])
                 (let ((ithrankvec (vector-ref newrts i)))
                   (when (> (vector-length ithrankvec) 1) (correct-rts-vec! newrts i))))

            ; find min
            (let ((minref (for/fold ([m #f])
                                    ([i (in-vector newrts)])
                                    (values (cond ((> (vector-length i) 0) 
                                                   (cond ((not (node? m)) (vector-ref i 0))
                                                         ((< (node-val (vector-ref i 0)) (node-val m)) (vector-ref i 0))
                                                         (else m)))
                                                  (else m))))))
              (set-fi-heap-minref! h minref)
              (set-fi-heap-roots! h newrts)
              (set-fi-heap-size! h (- (fi-heap-size h) 1)))))))

;; Returns a new heap after joining two heaps
;; Commentary:
;; - Doesn't change any nodes or any data of the heap provided as arguments
(define (fi-meld h1 h2)
  (cond ((not (fi-heap? h1)) (raise-argument-error 'fi-meld "fi-heap?" 0 h1 h2))
        ((not (fi-heap? h2)) (raise-argument-error 'fi-meld "fi-heap?" 1 h1 h2))
        (else
          (let ((rts (vector-append (for/vector ([i (in-vector (fi-heap-roots h1))]
                                                 [j (in-vector (fi-heap-roots h2))])
                                                (vector-append i j))
                                    (cond ((< (vector-length (fi-heap-roots h1)) (vector-length (fi-heap-roots h2)))
                                           (vector-take-right (fi-heap-roots h2) (vector-length (fi-heap-roots h1))))
                                          ((> (vector-length (fi-heap-roots h1)) (vector-length (fi-heap-roots h2)))
                                           (vector-take-right (fi-heap-roots h1) (vector-length (fi-heap-roots h2))))
                                          (else #()))))
                (minref (if (> (fi-findmin h1) (fi-findmin h2))
                          (fi-heap-minref h2)
                          (fi-heap-minref h1))))
            (fi-heap minref rts (+ (fi-heap-size h1) (fi-heap-size h2)))))))

;; Decrements a value of the node specified in the heap
;; Commentary:
;; - modifies the roots and min, if needed, of the heap argument 
;; - if heap condition is violated, then parent of the node is checked and if already marked, it is removed. This happens recursively
(define (fi-decrement! h noderef delta)
  (cond ((not (fi-heap? h)) (raise-argument-error 'fi-decrement! "fi-heap?" 0 h noderef delta))
        ((not (node? noderef)) (raise-argument-error 'fi-decrement! "node?" 1 h noderef delta))
        ((not (number? delta)) (raise-argument-error 'fi-decrement! "number?" 2 h noderef delta))
        (else (set-node-val! noderef (- (node-val noderef) delta))
              (when (< (node-val noderef) (fi-findmin h)) (set-fi-heap-minref! h noderef))

              ; heap condition violated
              (cond ((and (not (eq? (node-parent noderef) #f)) (< (node-val noderef) (node-val (node-parent noderef))))
                     (let ((nodernk (vector-length (node-children noderef)))
                           (parent (node-parent noderef)))
                       (set-node-parent! noderef #f)
                       (set-node-marked! noderef #f)
                       (vector-set! (fi-heap-roots h) nodernk (vector-append (vector-ref (fi-heap-roots h) nodernk) (vector noderef)))
                       (check-parents! h parent noderef))))))) 

;; Decrements noderef's value below the min of the heap, and calls deletemin
(define (fi-delete! h noderef)
  (cond ((not (fi-heap? h)) (raise-argument-error 'fi-delete! "fi-heap?" 0 h noderef))
        ((not (node? noderef)) (raise-argument-error 'fi-delete! "node?" 1 h noderef))

        ;if noderef to be deleted is the min node itself 
        (else (cond ((eq? noderef (fi-heap-minref h)) (fi-deletemin! h))
                    
                    ; delta := n - (m - 1)
                    ; m=0; n=1; delta=2
                    ; m=-1; n=0; delta=2
                    ; m=0; n=0; delta=1
                    ; m=-4; n=-3; delta=2, works!!!
                    (else (let ((delta (- (node-val noderef) (- (fi-findmin h) 1))))
                           (fi-decrement! h noderef delta)
                           (fi-deletemin! h)))))))

(define (fi-node-val n)
  (node-val n))

(define (fi-node-children n)
  (node-children n))

(define (fi-node-parent n)
  (node-parent n))
