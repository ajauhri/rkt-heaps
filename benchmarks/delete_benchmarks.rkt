#! /usr/bin/env racket
#lang racket
(require plot data/heap "../src/binomial.rkt" "../src/fibonacci.rkt" racket/cmdline "main_benchmarks.rkt")

(provide plot-findmin)

(define R 4294967087)

(define (fi-time-findmin h)
  (let ((start (current-inexact-milliseconds)) (r (fi-findmin h)) (end (current-inexact-milliseconds)))
   (cons (fi-insert h (random R)) (- end start))))

(define (create-findmin-timing-vec ssize esize heap-create-method time-method opts)
  (for/vector #:length (/ esize ssize) ([i (in-range ssize (+ esize ssize) ssize)])
              (let ((h (heap-create-method (make-rand-vector 1))))
               (/ (for/sum ([j (in-range 1 (+ i 1))]) 
                       (let ((res (time-method h)))
                        (set! h (car res))
                        (cdr res)))
                 (* ((car opts) i) i)))))

(define (plot-findmin ssize esize)
  (plot-file (list 
              (lines (get-plot-data ssize esize 
                                     create-findmin-timing-vec
                                     make-fi-heap 
                                     fi-time-findmin
                                     constant) #:color 2 #:label "fibonacci-findmin"  #:x-min (+ ssize ssize) #:style 'dot-dash))
             #:x-label "n" #:y-label "Average time (ms)/(constant)" (format "findmin_~a_~a.pdf" ssize esize) 'pdf))


(command-line 
  #:args
  (ssize esize) (plot-findmin (string->number ssize) (string->number esize)))
