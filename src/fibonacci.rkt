#lang racket 
;; Core functions that implement Fibonacci heaps. Developed by Michael L. Fredman and Robert E. Tarjan in 1984. Core functions and amortized costs are as follows:
;; makeheap     - creates a Fibonacci heap with the real value provided. Amortized cost is O(1)
;; findmin      - returns the minimum value in the heap. Amortized cost is O(1)
;; insert!      - adds a real value to the heap. Amortized cost is O(1)
;; deletemin!   - removes the reference of the node with the min value. Amortized cost is O(log n)
;; meld         - melds two heaps into a new heap, Amortized cost is O(1)
;; decrement!   - decrements the value of the node by delta. Amortized cost is O(1)
;; delete!      - deletes a node from the heap. Amortized cost is O(log n)

(require "fibonacci_helper.rkt")

(provide makeheap findmin insert! deletemin! meld decrement! delete! heap-minref heap-roots heap-size node-val node-children node-parent)

(define (makeheap val)
  (let ((n (node val #f #() #f)))
   (heap n (vector (vector n)) 1)))

(define (findmin h)
  (cond ((not (heap? h)) (raise-argument-error 'findmin "heap?" h))
        ((= (heap-size h) 0) (raise-type-error 'findmin "node" (heap-minref h)))
        (else (node-val (heap-minref h)))))

(define (insert! h val)
  (cond ((not (heap? h)) (raise-argument-error 'insert! "heap?" 0 h val))
        ((not (number? val)) (raise-argument-error 'insert! "number?" 1 h val))
        (else (let ((n (node val #f #() #f)))
               (when (= 0 (heap-size h)) (set-heap-minref! h n))
               (vector-set! (heap-roots h) 0 (vector-append (vector-ref (heap-roots h) 0) (vector n)))
               (set-heap-size! h (+ (heap-size h) 1))
               (when (and (> (heap-size h) 0) (< val (findmin h))) (set-heap-minref! h n))))))

;; Returns nothing. Modifies the heap provided and its nodes
;; Commentary:
;; - this procedure mutates node->parent & node->children of the previously defined nodes in the heap
;; - since deletemin is the only time root vectors are properly sorted as per their rank, we create a vector of size (log n) such that all nodes 
;;   (after finding the unique w.r.t. rank) are put in the right slot of the rank vector
(define (deletemin! h)
  (cond ((not (heap? h)) (raise-argument-error 'deletemin! "heap?" h))
        ((= (heap-size h) 0) (raise-type-error 'deletemin! "node" (heap-minref h)))
        (else 
          (let* ((maxrnk (+ 1 (inexact->exact (ceiling (/ (log (heap-size h)) (log 2))))))

                 ; put children of min into a root vector
                 (newrts (create-children-rts-vec (node-children (heap-minref h)) maxrnk))
                 (oldrts (heap-roots h)))

            ; combine other roots into the ^new root vector 
            (for ([i (in-range (vector-length newrts))])
                 (let ((ithrankvec1 (vec-ref oldrts i))
                       (ithrankvec2 (vec-ref newrts i)))
                   (vector-set! newrts i (vector-append ithrankvec1 ithrankvec2))))
            
            ; remove min node from newrts
            (let ((minrank (vector-length (node-children (heap-minref h))))
                  (minref (heap-minref h)))
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
              (set-heap-minref! h minref)
              (set-heap-roots! h newrts)
              (set-heap-size! h (- (heap-size h) 1)))))))

;; Returns a new heap after joining two heaps
;; Commentary:
;; - Doesn't change any nodes or any data of the heap provided as arguments
(define (meld h1 h2)
  (cond ((not (heap? h1)) (raise-argument-error 'meld "heap?" 0 h1 h2))
        ((not (heap? h2)) (raise-argument-error 'meld "heap?" 1 h1 h2))
        (else
          (let ((rts (vector-append (for/vector ([i (in-vector (heap-roots h1))]
                                                 [j (in-vector (heap-roots h2))])
                                                (vector-append i j))
                                    (cond ((< (vector-length (heap-roots h1)) (vector-length (heap-roots h2)))
                                           (vector-take-right (heap-roots h2) (vector-length (heap-roots h1))))
                                          ((> (vector-length (heap-roots h1)) (vector-length (heap-roots h2)))
                                           (vector-take-right (heap-roots h1) (vector-length (heap-roots h2))))
                                          (else #()))))
                (minref (if (> (findmin h1) (findmin h2))
                          (heap-minref h2)
                          (heap-minref h1))))
            (heap minref rts (+ (heap-size h1) (heap-size h2)))))))

;; Returns void.
;; Commentary:
;; - modifies the roots and min, if needed, of the heap argument 
(define (decrement! h noderef delta)
  (cond ((not (heap? h)) (raise-argument-error 'decrement! "heap?" 0 h noderef delta))
        ((not (node? noderef)) (raise-argument-error 'decrement! "node?" 1 h noderef delta))
        ((not (number? delta)) (raise-argument-error 'decrement! "number?" 2 h noderef delta))
        (else (set-node-val! noderef (- (node-val noderef) delta))
              (when (< (node-val noderef) (findmin h)) (set-heap-minref! h noderef))

              ; heap condition violated
              (cond ((and (not (eq? (node-parent noderef) #f)) (< (node-val noderef) (node-val (node-parent noderef))))
                     (let ((nodernk (vector-length (node-children noderef)))
                           (parent (node-parent noderef)))
                       (set-node-parent! noderef #f)
                       (set-node-marked! noderef #f)
                       (vector-set! (heap-roots h) nodernk (vector-append (vector-ref (heap-roots h) nodernk) (vector noderef)))
                       (check-parents! h parent noderef))))))) 

;; Decrements noderef's value below the min of the heap, and calls deletemin
(define (delete! h noderef)
  (cond ((not (heap? h)) (raise-argument-error 'delete! "heap?" 0 h noderef))
        ((not (node? noderef)) (raise-argument-error 'delete! "node?" 1 h noderef))

        ;if noderef to be deleted is the min node itself 
        (else (cond ((eq? noderef (heap-minref h)) (deletemin! h))
                    
                    ; delta := n - (m - 1)
                    ; m=0; n=1; delta=2
                    ; m=-1; n=0; delta=2
                    ; m=0; n=0; delta=1
                    ; m=-4; n=-3; delta=2, works!!!
                    (else (let ((delta (- (node-val noderef) (- (findmin h) 1))))
                           (decrement! h noderef delta)
                           (deletemin! h)))))))
