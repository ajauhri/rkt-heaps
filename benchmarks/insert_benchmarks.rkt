#! /usr/bin/env racket
#lang racket
(require plot data/heap "../src/binomial.rkt" "../src/fibonacci.rkt" racket/cmdline "main_benchmarks.rkt")

(provide plot-insert)

(define R 4294967087)

(define (bi-time-insert h)
  (let ((i (random R)))
   (let ((start (current-inexact-milliseconds)) (r (heap-add! h i)) (end (current-inexact-milliseconds)))
    (cons h (- end start)))))

(define (bino-time-insert h)
  (let ((i (random R)))
   (let ((start (current-inexact-milliseconds)) (r (bino-insert h i)) (end (current-inexact-milliseconds)))
    (cons r (- end start)))))

(define (fi-time-insert h)
  (let ((i (random R)))
   (let ((start (current-inexact-milliseconds)) (r (fi-insert! h i)) (end (current-inexact-milliseconds)))
    (cons r (- end start)))))


(define (create-insert-timing-vec ssize esize heap-create-method time-method opts)
  (for/vector #:length (/ esize ssize) ([i (in-range ssize (+ esize ssize) ssize)])
              (let ((h (heap-create-method (make-rand-vector 1))))
               (/ (for/sum ([j (in-range 1 (+ i 1))]) 
                       (let ((res (time-method h)))
                        (set! h (car res))
                        (cdr res)))
                 ((car opts) i)))))

(define (plot-insert ssize esize)
  (plot-file (list 
               ;(lines (get-plot-data ssize esize 
               ;                      create-insert-timing-vec
               ;                      make-bi-heap 
               ;                      bi-time-insert 
               ;                      logarithmic) #:color 4 #:label "binary-insert" #:x-min (+ ssize ssize) #:style 'dot)
               ;(lines (get-plot-data ssize esize 
               ;                      create-insert-timing-vec
               ;                      make-bino-heap 
               ;                      bino-time-insert 
               ;                      quadratic) #:color 1 #:label "binomial-insert (quadratic)" #:x-min (+ ssize ssize) #:style 'short-dash))
               (lines (get-plot-data ssize esize 
                                     create-insert-timing-vec
                                     make-bino-heap 
                                     bino-time-insert 
                                     linear) #:color 1 #:label "binomial-insert (linear)" #:x-min (+ ssize ssize) #:style 'long-dash))
               ;(lines (get-plot-data ssize esize
               ;                      create-insert-timing-vec
               ;                      make-fi-heap 
               ;                      fi-time-insert 
               ;                      linear) #:color 2 #:label "fibonacci-insert" #:x-min (+ ssize ssize) #:style 'dot-dash))
             #:x-label "n" #:y-label "Total time (ms)/(Total expected cost)" (format "insert_~a_~a.pdf" ssize esize) 'pdf))

(command-line 
  #:args
  (ssize esize) (plot-insert (string->number ssize) (string->number esize)))
