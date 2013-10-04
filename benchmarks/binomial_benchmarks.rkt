#! /usr/bin/env racket
#lang racket
(require plot "../src/binomial.rkt" racket/cmdline)

(provide plot-graphs)

(plot-new-window? #t)

(define (make-rand-vector s) 
  (for/vector #:length s ([i (in-range s)]) (random R)))


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
             #:x-label "n" #:y-label "Average time (ms)" (format "binomial_~a_~a_~a_~a.pdf" ssize esize step freq) 'pdf))

(command-line 
  #:args
  (ssize esize step freq) (plot-graphs (string->number ssize) (string->number esize) (string->number step) (string->number freq)))
