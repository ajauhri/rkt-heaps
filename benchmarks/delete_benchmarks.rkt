#! /usr/bin/env racket
#lang racket
(require plot data/heap "../src/binomial.rkt" "../src/fibonacci.rkt" racket/cmdline "main_benchmarks.rkt")

(provide plot-delete)

(define (fi-time-delete h [n (random-walk h (random (+ 1 (inexact->exact (ceiling (/ (log (fi-heap-size h)) (log 2)))))))])
  (let ((start (current-inexact-milliseconds)) (r (fi-delete! h n)) (end (current-inexact-milliseconds)))
   (cons h (- end start))))

(define (create-delete-timing-vec ssize esize heap-create-method time-method opts)
  (for/vector #:length (/ esize ssize) ([i (in-range ssize (+ esize ssize) ssize)])
              (let ((h (heap-create-method (make-rand-vector i))))
               (/ (for/sum ([j (in-range 1 (+ i 1))]) 
                       (let ((res (time-method h)))
                        (set! h (car res))
                        (cdr res)))
                 (* ((car opts) i) i)))))

(define (plot-delete ssize esize)
  (plot-file (list 
              (lines (get-plot-data ssize esize 
                                     create-delete-timing-vec
                                     make-fi-heap
                                     fi-time-delete
                                     logarithmic) #:color 2 #:label "fibonacci-delete"  #:x-min (+ ssize ssize) #:style 'dot-dash))
             #:x-label "n" #:y-label "Average time (ms)/(log n)" (format "delete_~a_~a.pdf" ssize esize) 'pdf))

(command-line 
  #:args
  (ssize esize) (plot-delete (string->number ssize) (string->number esize)))

