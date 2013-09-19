#lang racket

(require racket/mpair)
(provide make-node get-root-vector get-node-vector get-value heap? heaps? get-min-index)

(define (make-node val)
  (vector #f val #f (vector)))

(define (get-root-vector h)
  (mcdr h))

(define (get-node-vector h)
  (mcar (mcar h)))

(define (get-value n)
  (vector-ref n 1))

(define (get-min-index h)
  (mcdr (mcar h)))

(define (heap? h) #t)

(define (heaps? . h)
  (andmap heap? h))
