#lang racket 
;; Core functions that implement Fibonacci heaps. Developed by Michael L. Fredman and Robert E. Tarjan in 1984. Core functions and amortized costs are as follows:
;; fi-makeheap       - creates a Fibonacci heap with one value. Amortized cost is O(1)
;; fi-findmin        - returns the minimum value in the heap. Amortized cost is O(1)
;; fi-insert!        - adds a number value to the heap. Amortized cost is O(1)
;; fi-deletemin!     - removes the reference of the node with the min value. Amortized cost is O(log n)
;; fi-meld!          - melds two heaps into a new heap, Amortized cost is O(1)
;; fi-decrement!     - decrements the value of the node by delta. Amortized cost is O(1)
;; fi-delete!        - deletes a node from the heap. Amortized cost is O(log n)

;; Additional functions:
;; fi-heap-minref    - gives the reference to min node in the heap 
;; fi-heap-size      - gives the number of number values inserted in the heap
;; fi-node-val       - gives the value stored in the node
;; fi-node-children  - returns a vector of all child node references in the node
;; fi-node-parent    - returns reference to the parent node
;; fi-node-left      - returns reference to node's left node
;; fi-node-right     - returns reference to node's right node

(require "fibonacci_helper.rkt")

(provide fi-makeheap fi-findmin fi-insert! fi-deletemin! fi-meld! fi-decrement! fi-delete! fi-heap-minref fi-heap-size fi-node-val fi-node-children fi-node-parent fi-node-left fi-node-right (struct-out node) (struct-out fi-heap))

;; Returns a new fibonacci heap containing only one number
(define (fi-makeheap val)
  (cond ((not (number? val)) (raise-argument-error 'fi-makeheap "number?" val))
        (else (let ((n (node val #f #() #f #f #f)))
               (set-node-left! n n)
               (set-node-right! n n)
               (fi-heap n 1)))))

;; Returns the number in the min node of the heap
(define (fi-findmin h)
  (cond ((not (fi-heap? h)) (raise-argument-error 'fi-findmin "fi-heap?" h))
        ((= (fi-heap-size h) 0) (raise-type-error 'fi-findmin "node" (fi-heap-minref h)))
        (else (node-val (fi-heap-minref h)))))

;; Inserts a number to an existing heap
(define (fi-insert! h val)
  (cond ((not (fi-heap? h)) (raise-argument-error 'fi-insert! "fi-heap?" 0 h val))
        ((not (number? val)) (raise-argument-error 'fi-insert! "number?" 1 h val))
        ((= (fi-heap-size h) 0) (fi-makeheap val))
        (else (fi-meld! h (fi-makeheap val)))))

;; Modifies the heap provided and its nodes
;; Commentary:
;; - essential to find the min first and then merge trees with same rank such that the recursive method `correct-rts!`'s termination condition can be set
;; - operations makes sure that for a rank `i`, there is only one root node in the heap 
(define (fi-deletemin! h)
  (cond ((not (fi-heap? h)) (raise-argument-error 'fi-deletemin! "fi-heap?" h))
        ((= (fi-heap-size h) 0) (raise-type-error 'fi-deletemin! "node" (fi-heap-minref h)))
        ((= (fi-heap-size h) 1) (set-fi-heap-minref! h #f) (set-fi-heap-size! h 0))
        (else 
          (let* ((maxrnk (+ 1 (inexact->exact (ceiling (/ (log (fi-heap-size h)) (log 2))))))
                 (rtsvec (make-vector maxrnk (vector)))
                 (minref (fi-heap-minref h))
                 (minchildren (node-children minref)))
            (for ([i (in-vector minchildren)])
                 (add-node-to-dll! h i))
            (set-fi-heap-minref! h (node-left minref)) ;set an arbitrary node as min
            (remove-node-from-dll! h minref)
            
            (define (get-min h noderef minref)
              (cond ((eq? noderef (fi-heap-minref h)) minref)
                    ((< (node-val noderef) (node-val minref)) 
                     (get-min h (node-right noderef) noderef))
                    (else (get-min h (node-right noderef) minref))))
            
            (set-fi-heap-minref! h (get-min h (node-right (fi-heap-minref h)) (fi-heap-minref h)))
            (set-fi-heap-size! h (- (fi-heap-size h) 1))     

            (define (correct-rts! h noderef rtsvec)
              (let* ((nodernk (vector-length (node-children noderef)))
                     (nodernkvec (vector-ref rtsvec nodernk)))
                (cond ((> (vector-length nodernkvec) 0)
                       (let ((nodeuped (combine! h noderef (vector-ref nodernkvec 0))))
                        (vector-set! rtsvec nodernk (vector-drop nodernkvec 1))
                        (correct-rts! h nodeuped rtsvec)))
                      ((= (vector-length nodernkvec) 0) 
                       (vector-set! rtsvec nodernk (vector noderef))
                       (cond ((eq? (node-right noderef) (fi-heap-minref h)) (void))
                             (else (correct-rts! h (node-right noderef) rtsvec)))))))

            (correct-rts! h (fi-heap-minref h) rtsvec)))))


;; Returns a new heap after joining two heaps
(define (fi-meld! h1 h2)
  (cond ((not (fi-heap? h1)) (raise-argument-error 'fi-meld! "fi-heap?" 0 h1 h2))
        ((not (fi-heap? h2)) (raise-argument-error 'fi-meld! "fi-heap?" 1 h1 h2))
        (else
          (let* ((h1min (fi-heap-minref h1))
                 (h2min (fi-heap-minref h2))
                 (h1minright (node-right h1min))
                 (h2minleft (node-left h2min))
                 (minref (if (> (fi-findmin h1) (fi-findmin h2))
                           h2min
                           h1min)))
            (set-node-right! h1min h2min)
            (set-node-left! h2min h1min)
            (set-node-left! h1minright h2minleft)
            (set-node-right! h2minleft h1minright)
            (fi-heap minref (+ (fi-heap-size h1) (fi-heap-size h2)))))))

;; Decrements a value of the node specified in the heap
;; Commentary:
;; - if heap condition is violated, then parent of the node is checked and if already marked, it is removed. This happens recursively
(define (fi-decrement! h noderef delta)
  (cond ((not (fi-heap? h)) (raise-argument-error 'fi-decrement! "fi-heap?" 0 h noderef delta))
        ((not (node? noderef)) (raise-argument-error 'fi-decrement! "node?" 1 h noderef delta))
        ((not (number? delta)) (raise-argument-error 'fi-decrement! "number?" 2 h noderef delta))
        (else (set-node-val! noderef (- (node-val noderef) delta))

              ; heap condition violated
              (cond ((and (not (eq? (node-parent noderef) #f)) (< (node-val noderef) (node-val (node-parent noderef))))
                     (let* ((parent (node-parent noderef)))
                      (add-node-to-dll! h noderef)
                      (set-node-parent! noderef #f)
                      (set-node-marked! noderef #f)
                      (check-parents! h parent noderef)
                      (when (< (node-val noderef) (fi-findmin h)) (set-fi-heap-minref! h noderef)))))))) 

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

(define (fi-node-left n)
  (node-left n))

(define (fi-node-right n)
  (node-right n))
