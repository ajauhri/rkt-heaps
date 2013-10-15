#! /usr/bin/env racket
#lang racket
(require plot data/heap "../src/binomial.rkt" "../src/fibonacci.rkt" racket/cmdline "main_benchmarks.rkt")

(provide plot-decrement)

(define R 4294967087)

(define (fi-time-decrement h [n (random-walk h (random (+ 1 (inexact->exact (ceiling (/ (log (fi-heap-size h)) (log 2)))))))] [delta (random R)])
  (let ((start (current-inexact-milliseconds)) (r (fi-decrement! h n delta)) (end (current-inexact-milliseconds)))
   (cons h (- end start))))

(define (create-decrement-timing-vec ssize esize heap-create-method time-method opts)
  (for/vector #:length (/ esize ssize) ([i (in-range ssize (+ esize ssize) ssize)])
              (let ((h (heap-create-method (make-rand-vector i))))
               (/ (for/sum ([j (in-range 1 (+ i 1))]) 
                       (let ((res (time-method h)))
                        (set! h (car res))
                        (cdr res)))
                 ((car opts) i)))))

(define (plot-decrement ssize esize)
  (plot-file (list 
              (lines (get-plot-data ssize esize 
                                     create-decrement-timing-vec
                                     make-fi-heap
                                     fi-time-decrement
                                     linear) #:color 2 #:label "fibonacci-decrement"  #:x-min (+ ssize ssize) #:style 'dot-dash))
             #:x-label "n" #:y-label "Total time (ms)/(Total expected time)" (format "decrement_~a_~a.pdf" ssize esize) 'pdf))


(command-line 
  #:args
  (ssize esize) (plot-decrement (string->number ssize) (string->number esize)))
