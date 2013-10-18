#! /usr/bin/env racket
#lang racket
(require plot data/heap "../src/binomial.rkt" "../src/fibonacci.rkt" racket/cmdline "main_benchmarks.rkt")

(provide plot-meld)

(define R 4294967087)

(define (bi-time-meld h1 h2)
  (let ((start (current-inexact-milliseconds)) (r (heap-add-all! h1 h2)) (end (current-inexact-milliseconds)))
   (cons r (- end start))))

(define (bino-time-meld h1 h2)
  (let ((start (current-inexact-milliseconds)) (r (bino-meld h1 h2)) (end (current-inexact-milliseconds)))
   (cons r (- end start))))

(define (fi-time-meld h1 h2)
  (let ((start (current-inexact-milliseconds)) (r (fi-meld! h1 h2)) (end (current-inexact-milliseconds)))
   (cons r (- end start))))

(define (create-meld-timing-vec ssize esize heap-create-method time-method opts)
  (for/vector #:length (/ esize ssize) ([i (in-range ssize (+ esize ssize) ssize)])
              (/ (for/sum ([j (in-range 2 (+ i 1))])
                          (let* ((h1 (heap-create-method (make-rand-vector (ceiling (/ j 2)))))
                                 (h2 (heap-create-method  (make-rand-vector (ceiling (/ (- j 1) 2)))))
                                 (res (time-method h1 h2)))
                            (cdr res)))
                 ((car opts) i))))

(define (plot-meld ssize esize)
  (plot-file (list 
               ;(lines (get-plot-data ssize esize 
               ;                      create-meld-timing-vec
               ;                      make-bi-heap 
               ;                      bi-time-meld
               ;                      logarithmic) #:color 4 #:label "binary-meld" #:x-min (+ ssize ssize) #:style 'dot)
               ;(lines (get-plot-data ssize esize 
               ;                      create-meld-timing-vec
               ;                      make-bino-heap 
               ;                      bino-time-meld
               ;                      logarithmic) #:color 1 #:label "binomial-meld (eager)" #:x-min (+ ssize ssize) #:style 'short-dash)
               (lines (get-plot-data ssize esize
                                     create-meld-timing-vec
                                     make-fi-heap 
                                     fi-time-meld
                                     linear) #:color 2 #:label "fibonacci-meld" #:x-min (+ ssize ssize) #:style 'dot-dash))
             #:x-label "n" #:y-label "Total time (ms)/(Total expected cost)" (format "meld_~a_~a.pdf" ssize esize) 'pdf))

(command-line 
  #:args
  (ssize esize) (plot-meld (string->number ssize) (string->number esize)))
