#! /usr/bin/env racket
#lang racket
(require plot "../src/binomial.rkt" racket/cmdline)

(provide plot-graphs)

(plot-new-window? #t)

(define R 4294967087)

(define (make-rand-vector s) 
  (for/vector #:length s ([i (in-range s)]) (random R)))

(define (make-large-heap v)
  (for/fold ([h (makeheap (random R))]) ([val (in-vector v)]) (insert h val)))

(define (time-findmin h)
  (let ((start (current-inexact-milliseconds)) (r (findmin h)) (end (current-inexact-milliseconds)))
   (- end start))) 

(define (time-deletemin h)
  (let ((start (current-inexact-milliseconds)) (r (deletemin h)) (end (current-inexact-milliseconds)))
   (- end start)))

(define (time-insert h [i (random R)])
  (let ((start (current-inexact-milliseconds)) (r (insert h i)) (end (current-inexact-milliseconds)))
   (- end start)))

(define (time-meld h1 h2)
  (let ((start (current-inexact-milliseconds)) (r (meld h1 h2)) (end (current-inexact-milliseconds)))
   (- end start)))

(define (create-timing-vector time-method ssize esize step freq)
  (cond ((eq? time-method time-meld)
         (for/vector #:length (/ (- esize ssize) step) ([i (in-range ssize esize step)])
                     (let ((h1 (make-large-heap (make-rand-vector (ceiling (/ (- i 1) 2)))))
                           (h2 (make-large-heap (make-rand-vector (ceiling (/ (- i 1) 2))))))
                       (/ (for/sum ([j (in-range freq)]) (time-method h1 h2)) freq))))
        (else 
          (for/vector #:length (/ (- esize ssize) step) ([i (in-range ssize esize step)])
                      (let ((h (make-large-heap (make-rand-vector (- i 1)))))
                       (/ (for/sum ([j (in-range freq)]) (time-method h)) freq))))))

(define (get-plot-data time-method ssize esize step freq)
  (reverse
    (let ((v (create-timing-vector time-method ssize esize step freq)))
     (for/fold ([lst (list (vector 0 0))]) ([i (in-range ssize esize step)]
                                            [j (in-range (vector-length v))])
               (cons (vector i (vector-ref v j)) lst)))))

(define (plot-graphs ssize esize step freq)
  (plot-file (list 
               (lines (get-plot-data time-findmin ssize esize step freq) #:color 1 #:label "findmin" #:style 'dot)
               (lines (get-plot-data time-deletemin ssize esize step freq) #:color 2 #:label "deletemin" #:style 'solid)
               (lines (get-plot-data time-insert ssize esize step freq) #:color 3 #:label "insert" #:style 'long-dash) 
               (lines (get-plot-data time-meld ssize esize step freq) #:color 0 #:label "meld" #:style 'dot-dash)) 
             #:x-label "n" #:y-label "Average time (ms)" (format "binomial_~a_~a_~a_~a" ssize esize step freq) 'pdf))

(command-line 
  #:args
  (ssize esize step freq) (plot-graphs (string->number ssize) (string->number esize) (string->number step) (string->number freq)))
