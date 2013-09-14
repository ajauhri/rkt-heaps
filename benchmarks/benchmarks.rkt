#! /usr/bin/racket
#lang racket

(require plot "../src/binomial.rkt")

(define R 4294967087)

(define (make-rand-vector s) 
  (for/vector #:length s ([i (in-range s)]) (random R)))

(define (b-makeheap v)
  (for/fold ([h (makeheap (random R))]) ((val (in-vector v))) (insert h val)))
