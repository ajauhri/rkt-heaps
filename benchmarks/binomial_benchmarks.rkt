#! /usr/bin/racket
#lang racket

(require plot "../src/binomial.rkt")

(define R 4294967087)

(define (make-rand-vector s) 
  (for/vector #:length s ([i (in-range s)]) (random R)))

(define (make-large-heap v)
  (for/fold ([h (makeheap (random R))]) ((val (in-vector v))) (insert h val)))

(define (b-makeheap v)
  (let ((start (current-inexact-milliseconds)) (r (makeheap v)) (end (current-inexact-milliseconds)))
   (- end start)))
 
(define (b-findmin h)
  (let ((start (current-inexact-milliseconds)) (r (findmin h)) (end (current-inexact-milliseconds)))
   (- end start))) 

(define (b-insert v)
  (time
    (for/fold ([h (makeheap (random R))]) ([val (in-vector v)])
            (insert h val))))

(define (b-deletemin h)
  (let ((start (current-inexact-milliseconds)) (r (deletemin h)) (end (current-inexact-milliseconds)))
   (- end start)))

(define (b-meld h1 h2)
  (let ((start (current-inexact-milliseconds)) (r (meld h1 h2)) (end (current-inexact-milliseconds)))
   (- end start)))
