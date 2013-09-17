#lang racket

(require racket/mpair)
(provide make-node get-roots get-node-vector get-value heap?)

(define (make-node val)
  (vector #f val #f (vector)))

(define (get-roots h)
  (mcdr h))

(define (get-node-vector h)
  (mcar (mcar h)))

(define (get-value n)
  (vector-ref n 1))

(define (heap? h) #t)
