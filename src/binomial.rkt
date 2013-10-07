#lang racket
;; Core functions that implement Binomial heaps. Developed by J. Vuillemin in 1978. They are a special case of Fibonacci heaps. This is a pure functional implementation. Core functions and amortized costs are as follows:
;; bino-makeheap     - creates a Binomial heap with the number value provided. Amortized cost is O(1)
;; bino-findmin      - returns the minimum value in the heap. Amortized cost is O(1) 
;; bino-insert       - adds a number value to a copy of the heap provided as argument. Since the heap is not mutated, the cost is not constant as otherwise could have been had it not copy the existing the heap.
;; bino-deletemin    - removes the min value from the new heap provided. Amortized cost is O(log n)
;; bino-meld         - melds two heaps into a new heap. This is the eager version of meld as described by Kozen. Amortized cost O(log n)

;; Additional functions:
;; bino-count        - gives the number of number values inserted in the heap. Cost - O(1)

(require "binomial_helper.rkt")

(provide bino-makeheap bino-findmin bino-insert bino-deletemin bino-meld bino-count)

;; Returns a new binomial heap containing only one number
;; Commentary:
;; - the heap is structured as a vector along with number of values as a pair with `car` pointing to the vector and `cdr` to the number of values in the heap. To make findmin a constant time operation, the min is stored as the `cdr` of another pair.
(define (bino-makeheap val)
  (cond ((not (number? val)) (raise-argument-error 'bino-makeheap "number?" val))
        (else (cons (cons (vector val) 1) val))))

;; Returns the min value in the heap
;; Commentary: 
;;- vector-ref is takes constant time - www.eecs.berkeley.edu/~bh/ssch23/vectors.html 
;;- if min is not provided or argument not heap. #f is returned 
(define (bino-findmin h)
  (cond ((not (heap-lazy? h)) (raise-argument-error 'bino-findmin "heap-lazy?" h))
        (else (cdr h))))

;; Inserts a number to an existing heap and returns the resultant heap
(define (bino-insert h val)
  (cond ((not (heap-lazy? h)) (raise-argument-error 'bino-insert "heap-lazy?" 0 h val))
        ((not (number? val)) (raise-argument-error 'bino-insert "number?" 1 h val))
        (else (bino-meld h (bino-makeheap val)))))

;; Deletes the root of the tree with the min value in heap. Melds the remaining values
(define (bino-deletemin h)
  (cond ((not (heap? h)) (raise-argument-error 'bino-deletemin "heap?" h))
        ((eq? (cdr h) #f) (raise-user-error "Min value not specified or corrupt heap structure. Given min in heap " (cdr h)))
        (else (let* ((min-index (getmin (car (car h)) (cdr (car h)) 1 #f))
                     (orig-vec (vector-copy (car (car h))))
                     (count (cdr (car h)))
                     (tree-start (+ 1 min-index))
                     (tree-end (+ 1 (* min-index 2)))
                     (tree (vector-copy orig-vec tree-start tree-end)))
                (vector-copy! orig-vec min-index (make-vector (+ min-index 1) #f))  ; put #f in place of the tree rooted at min
                (cond ((= count 1) (cons (cons #() 0) #f))
                      (else (bino-meld (cons (cons orig-vec (- count (+ min-index 1))) 
                                        (vec-ref orig-vec (getmin orig-vec (- count (+ min-index 1)) 1 #f)))
                                  (cons (cons tree (- tree-end tree-start))
                                        (vec-ref tree (getmin tree (- tree-end tree-start) 1 #f))))))))))


;; Returns a heap which a combination of the two heaps provided as arguments to the method. 
(define (bino-meld h1 h2)
  (cond ((not (heap-lazy? h1)) (raise-argument-error 'bino-meld "heap-lazy?" 0 h1 h2))
        ((not (heap-lazy? h2)) (raise-argument-error 'bino-meld "heap-lazy?" 1 h1 h2))
        (else (define (couple v1 v2 s1 s2 carry res i)
                (let ((b1 (root-slot-valid? s1)) 
                      (b2 (root-slot-valid? s2)) 
                      (b3 (if (> (vector-length carry) 0) #t #f)))
                  (cond ((and (= (min s1 s2) 0) (not b3)) (vector-append res (cond ((and (= s1 0) (= s2 0) #()))
                                                                                   ((= s1 0) (vector-copy v2 (- i 1) (vector-length v2)))
                                                                                   ((= s2 0) (vector-copy v1 (- i 1) (vector-length v1))))))
                        (else (let ((newargs (cond ((and (not b1) (not b2) (not b3)) 
                                                    (cons #() (vector-append res (make-vector i #f))))
                                                   ((and (not b1) (not b2) b3) 
                                                    (cons #() (vector-append res carry))) 
                                                   ((and (not b1) b2 (not b3)) 
                                                    (cons #() (vector-append res (propres v2 i))))
                                                   ((and (not b1) b2 b3) 
                                                    (cons (propcarry v2 carry i) (vector-append res (make-vector i #f))))
                                                   ((and b1 (not b2) (not b3)) 
                                                    (cons #() (vector-append res (propres v1 i))))
                                                   ((and b1 (not b2) b3) 
                                                    (cons (propcarry v1 carry i) (vector-append res (make-vector i #f))))
                                                   ((and b1 b2 (not b3)) 
                                                    (cons (constructcarry v1 v2 i) (vector-append res (make-vector i #f))))
                                                   ((and b1 b2 b3) 
                                                    (cons (propcarry v2 carry i) (vector-append res (propres v1 i)))))))
                                (couple v1 v2 
                                        (floor (/ s1 2)) 
                                        (floor (/ s2 2)) 
                                        (car newargs) 
                                        (cdr newargs) 
                                        (* i 2)))))))
              (let ((res (cons (couple (car (car h1)) 
                                       (car (car h2)) 
                                       (cdr (car h1)) 
                                       (cdr (car h2)) #() #() 1) 
                               (+ (cdr (car h1)) (cdr (car h2)))))
                    (minh1 (bino-findmin h1))
                    (minh2 (bino-findmin h2)))
                (cons res (cond ((eq? #f minh1) minh2)
                                ((eq? #f minh2) minh1)
                                ((< minh1 minh2) minh1)
                                (else minh2)))))))

;; Returns the count of elements in the heap. The vector size for storing the all elements may be greater than the count.
(define (bino-count h) 
  (cond ((not (heap-lazy? h)) (raise-argument-error 'count "heap-lazy?" h)) 
        (else (cdr (car h)))))
