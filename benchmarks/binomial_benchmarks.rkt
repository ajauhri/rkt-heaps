#! /usr/bin/racket
#lang racket
(require plot "../src/binomial.rkt")
(provide run-findmin plot-findmin)

(define R 4294967087)

(define (make-rand-vector s) 
  (for/vector #:length s ([i (in-range s)]) (random R)))

(define (make-large-heap v)
  (for/fold ([h (makeheap (random R))]) ([val (in-vector v)]) (insert h val)))

(define (time-makeheap v)
  (let ((start (current-inexact-milliseconds)) (r (makeheap v)) (end (current-inexact-milliseconds)))
   (- end start)))
 
(define (time-findmin h)
  (let ((start (current-inexact-milliseconds)) (r (findmin h)) (end (current-inexact-milliseconds)))
   (- end start))) 

(define (time-insert h i)
  (let ((start (current-inexact-milliseconds)) (r (insert h i)) (end (current-inexact-milliseconds)))
   (- end start)))

(define (time-deletemin h)
  (let ((start (current-inexact-milliseconds)) (r (deletemin h)) (end (current-inexact-milliseconds)))
   (- end start)))

(define (time-meld h1 h2)
  (let ((start (current-inexact-milliseconds)) (r (meld h1 h2)) (end (current-inexact-milliseconds)))
   (- end start)))

(define (run-findmin ssize esize step freq)
  (for/vector #:length (/ (- esize ssize) step) ([i (in-range ssize esize step)])
              (let ((h (make-large-heap (make-rand-vector (- i 1)))))
               (/ (for/sum ([j (in-range freq)]) (time-findmin h)) freq))))

(define (plot-findmin ssize esize step freq)
  (plot (lines
          (reverse
           (let ((v (run-findmin ssize esize step freq)))
           (for/fold ([lst (list (vector 0 0))]) ([i (in-range ssize esize step)]
                                                 [j (in-range (vector-length v))])
                     (cons (vector i (vector-ref v j)) lst)))))))
