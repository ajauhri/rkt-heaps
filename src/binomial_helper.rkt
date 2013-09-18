#lang racket
;;;
;;; Helper functions
;;;

(provide getmin heap? heaps? heap-lazy? propres propcarry constructcarry root-slot-valid? vec-ref)

(define (getmin v len i m) 
  (if (= len 0)
    m
    (getmin v (floor (/ len 2)) (* i 2) (cond ((and (= (modulo len 2) 1) (eq? m #f)) (- i 1))
                                              ((= (modulo len 2) 0) m)
                                              (else (if (< (vector-ref v (- i 1)) (vector-ref v m)) 
                                                      (- i 1) 
                                                      m))))))

;; Predicate based on the structural formation of binomial heap. 
(define (heaps? . h)
  (andmap heap-lazy? h))

;; Asserts whether the given argument is a valid heap or not
;; Commentary:
;; - based on the underlying implementation structural formation of a heap, it checks whether they are correctly specified in the argument
;; - it is checked whether the values of the heap are stored in a vector
;; - it is checked that the index of the min is either not specified or if specified, it should be correct
;; - it is checked that the number of valid elements in the heap are at most the length of the entire forest(vector) 
;; - doesn't check whether the heap condition is valid for nodes of the forest
;; - it is fine to have #f stored as min. findmin and deletemin will make checks if that is the case and return unspecified values
;; - Why is #f allowed in the cdr of the pair? 
;;   deletemin which subsequently calls meld, could provide a heap which does not have the min identified. Example: a heap with four elements, will have a vector #(#f #f #f)
;; - this check is not thorough
(define (heap? h)
  (and (vector? (car (car h))) 
       (or (eq? (cdr h) #f) (= (cdr h) (vector-ref (car (car h)) (getmin (car (car h)) (cdr (car h)) 1 #f))))
       (<= (cdr (car h)) (vector-length (car (car h))))))

(define (heap-lazy? h)
  (and (vector? (car (car h)))
       (or (eq? (cdr h) #f) (>= (cdr h) 0))
       (<= (cdr (car h)) (vector-length (car (car h))))))

;; Returns ith bit of v
(define (propres v i)
  (vector-copy v (- i 1) (- (* i 2) 1)))

;; Adds ith bit of v to the carry for propagation. Maintains the heap condition
(define (propcarry v c i) 
  (if (<= (vector-ref c 0) (vector-ref v (- i 1))) 
    (vector-append c (vector-copy v (- i 1) (- (* i 2) 1)))
    (vector-append (vector-copy v (- i 1) (- (* i 2) 1)) c)))

;; Constructs ith bit of v1 and v2 as carry for propagation. The construction maintains the heap condition.
(define (constructcarry v1 v2 i)
  (let ((start (- i 1)) (end (- (* i 2) 1)))
   (if (<= (vector-ref v1 start) (vector-ref v2 start))
     (vector-append (vector-copy v1 start end) (vector-copy v2 start end))
     (vector-append (vector-copy v2 start end) (vector-copy v1 start end)))))

;; Returns whether the root of a tree in the heap is vacant or not
(define (root-slot-valid? s)
  (cond ((= (modulo s 2) 1) #t)
        (else #f))) 

(define (vec-ref vec pos)
  (cond ((eq? #f pos) #f)
        ((>= pos (vector-length vec)) #f)
        (else (vector-ref vec pos))))
