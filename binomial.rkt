#lang racket
(require racket/trace)
;; Book-keeper of the functions to be implemented 
;; makeheap(i) 
;; findmin(h) 
;; insert(h,i)  
;; deletemin(h) 
;; meld(h,h') 

;; Returns a new heap containing only element
;; Commentary:
;; - the heap is structured as a vector along with number of values as a pair with `car` pointing to the vector and `cdr` to the number of values in the heap. To make min a constant time operation, the index to the min (root of a tree in the forest) is also stored as the `cdr` of another pair.
(define (makeheap i)
  (cons (cons (make-vector 1 i) 1) 0))

;; Returns the min element in the heap
;; Commentary: 
;;- vector-ref is takes constant time - www.eecs.berkeley.edu/~bh/ssch23/vectors.html 
;;- if min is not provided or argument not heap. #f is returned 
(define (findmin h)
  (if (and (heap? h) (not (eq? (cdr h) #f)))                    ;#f check is necessary since heap? is fine with it
    (vector-ref (car (car h)) (cdr h))
    #f))

;; Insert a positive integer to an existing heap and returns the resultant heap
(define (insert h i)
  (if (heap? h)
    (meld h (makeheap i))
    #f))

;; Deletes the root of the tree with the min value in heap. Melds the remaining values
(define (deletemin h)
  (if (and (heap? h) (not (eq? (cdr h) #f)))                   
    (let ((min-index (cdr h))
          (orig-vec (vector-copy (car (car h))))
          (count (cdr (car h)))
          (tree-start (+ 1 (cdr h)))
          (tree-end (+ 1 (* (cdr h) 2))))
      (let ((tree (vector-copy orig-vec tree-start tree-end)))
       (vector-copy! orig-vec min-index (make-vector (+ min-index 1) #f)) 
       (if (= (vector-length orig-vec) 1)
         (cons (cons #() 0) #f)
         (meld 
           (cons (cons orig-vec (- count (+ min-index 1))) #f)
           (cons (cons tree (- tree-end tree-start)) #f)))))
    #f))

;; Returns a heap which a combination of the two heaps provided as arguments to the method. 
(define (meld h1 h2)
  (if (heaps? h1 h2)
    (let ()
     (define (couple v1 v2 s1 s2 carry res i)
       (let ((b1 (root-slot-valid? s1)) 
             (b2 (root-slot-valid? s2)) 
             (b3 (if (> (vector-length carry) 0) #t #f)))
         (cond ((and (= s1 0) (= s2 0) (not b3)) res)
               (else
                 (let ((newargs 
                         (cond ((and (not b1) (not b2) (not b3)) (cons #() (vector-append res (make-vector i #f))))
                               ((and (not b1) (not b2) b3) (cons #() (vector-append res carry))) 
                               ((and (not b1) b2 (not b3)) (cons #() (vector-append res (propres v2 i))))
                               ((and (not b1) b2 b3) (cons (propcarry v2 carry i) (vector-append res (make-vector i #f))))
                               ((and b1 (not b2) (not b3)) (cons #() (vector-append res (propres v1 i))))
                               ((and b1 (not b2) b3) (cons (propcarry v1 carry i) (vector-append res (make-vector i #f))))
                               ((and b1 b2 (not b3)) (cons (constructcarry v1 v2 i) (vector-append res (make-vector i #f))))
                               ((and b1 b2 b3) (cons (propcarry v2 carry i) (vector-append res (propres v1 i)))))))
                   (couple v1 v2 (floor (/ s1 2)) (floor (/ s2 2)) (car newargs) (cdr newargs) (* i 2)))))))
     (let ((res (cons (couple (car (car h1)) (car (car h2)) (cdr (car h1)) (cdr (car h2)) #() #() 1) 
                      (+ (cdr (car h1)) (cdr (car h2))))))
       (cons res 
             (getmin (car res) (cdr res) 1 #f))))
    #f))

;;;
;;; Helper functions
;;;
(define (getmin v len i m) 
  (if (= len 0)
    (- m 1)
    (getmin v (floor (/ len 2)) (* i 2) (cond ((and (= (modulo len 2) 1) (eq? m #f)) i)
                                              ((= (modulo len 2) 0) m)
                                              (else
                                                (if (< (vector-ref v (- i 1)) (vector-ref v (- m 1))) i m))))))

;; Predicate based on the structural formation of binomial heap.
(define (heaps? . h)
  (andmap heap? h))

;; Asserts whether the given argument is a valid heap or not
;; Commentary:
;; - based on the underlying implementation structural formation of a heap, it checks whether they are correctly specified in the argument
;; - it is checked whether the values of the heap are stored in a vector
;; - it is checked that the index of the min is either not specified or if specified, it should be correct
;; - it is checked that the number of valid elements in the heap are at most the length of the entire forest(vector) 
;; - doesn't check whether the heap condition is maintained or not
;; - it is fine to have #f stored as min. findmin and deletemin will make checks if that is the case and return unspecified values
;; - Why is #f considered invalid? deletemin which subsequently calls meld, could provide a heap which does not have the min identified. Example: a heap with four elements, will have a vector #(#f #f #f)
;; - this check is not thorough
(define (heap? h)
  (and (vector? (car (car h))) 
       (or (eq? (cdr h) #f) (= (cdr h) (getmin (car (car h)) (cdr (car h)) 1 #f)))
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
        ((= (modulo s 2) 0) #f))) 
