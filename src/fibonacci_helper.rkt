#lang racket

(require racket/mpair)
(provide make-node get-roots get-vector heap?)

(define (make-node val)
  (mcons val (mcons #f (vector))))

(define (get-roots h)
  (mcdr h))

(define (get-vector h)
  (mcar (mcar h)))

(define (get-value v)
  (mcons v))

(define (heap? h) #t)
