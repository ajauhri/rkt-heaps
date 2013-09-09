#lang racket
;; Book-keeper of the functions to be implemented 
;; makeheap(i) 
;; findmin(h) 
;; insert(h,i)  
;; deletemin(h) 
;; meld(h,h') 

(require "helper.rkt")

(provide makeheap findmin insert deletemin meld)

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

