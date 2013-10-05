#! /usr/bin/env racket
#lang racket
(require plot data/heap "../src/binomial.rkt" "../src/fibonacci.rkt" racket/cmdline "main_benchmarks.rkt")

(provide plot-deletemin)

(define R 4294967087)

(define (bi-time-deletemin h)
  (let ((start (current-inexact-milliseconds)) (r (heap-remove-min! h)) (end (current-inexact-milliseconds)))
   (cons h (- end start))))

(define (bino-time-deletemin h)
  (let ((start (current-inexact-milliseconds)) (r (bino-deletemin h)) (end (current-inexact-milliseconds)))
   (cons r (- end start))))

(define (fi-time-deletemin h)
  (let ((start (current-inexact-milliseconds)) (r (fi-deletemin! h)) (end (current-inexact-milliseconds)))
   (cons h (- end start))))

(define (create-deletemin-timing-vec ssize esize heap-create-method time-method opts)
  (for/vector #:length (/ esize ssize) ([i (in-range ssize (+ esize ssize) ssize)])
              (let ((h (heap-create-method (make-rand-vector i))))
               (/ (for/sum ([j (in-range 1 (+ i 1))]) 
                       (let ((res (time-method h)))
                        (set! h (car res))
                        (cdr res)))
                 (* ((car opts) i) i)))))

(define (plot-deletemin ssize esize)
  (plot-file (list 
               (lines (get-plot-data ssize esize 
                                     create-deletemin-timing-vec 
                                     make-bi-heap 
                                     bi-time-deletemin 
                                     logarithmic) #:color 4 #:label "binary-deletemin" #:x-min (+ ssize ssize) #:style 'dot)
               (lines (get-plot-data ssize esize 
                                     create-deletemin-timing-vec
                                     make-bino-heap 
                                     bino-time-deletemin 
                                     logarithmic) #:color 1 #:label "binomial-deletemin"  #:x-min (+ ssize ssize) #:style 'short-dash)
               (lines (get-plot-data ssize esize 
                                     create-deletemin-timing-vec
                                     make-scraggly-heap 
                                     fi-time-deletemin 
                                     logarithmic) #:color 2 #:label "fibonacci-deletemin"  #:x-min (+ ssize ssize) #:style 'dot-dash))
             #:x-label "n" #:y-label "Average time (ms)/(log n)" (format "deletemin_~a_~a.pdf" ssize esize) 'pdf))


(command-line 
  #:args
  (ssize esize) (plot-deletemin (string->number ssize) (string->number esize)))
