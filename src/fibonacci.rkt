#lang racket 
;; Book-keeper fof the functions to be implemented 
;; makeheap
;; findmin
;; insert
;; deletemin
;; meld
;; decrement
;; delete
(require racket/mpair "fibonacci_helper.rkt")
(provide makeheap findmin)

(define (makeheap val)
  (mcons 
    (mcons 
      (make-vector 1 (make-node val)) 
      0) 
    (make-hash)))

(define (findmin h)
  (if (heap? h)
    (vector-ref (get-vector h) (mcdr (mcar h)))
    #f))

