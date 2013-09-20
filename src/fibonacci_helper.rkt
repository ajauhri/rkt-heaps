#lang racket

(provide make-node get-root-vector get-node-vector get-value get-min-index heap? make-root-vector)

(define (make-node val)
  (vector #f val #f (vector)))

(define (get-root-vector h)
  (cdr h))

(define (get-node-vector h)
  (car (car h)))

(define (get-value n)
  (vector-ref n 1))

(define (get-min-index h)
  (cdr (car h)))

(define (heap? h) #t)

(define (make-root-vector h1 h2 )
  (let ((rootvech1 (get-root-vector h1))
        (rootvech2 (get-root-vector h2))
        (sizeh1 (vector-length (get-node-vector h1))))
    (vector-append (for/vector ([i (in-vector rootvech1)]
                                [j (in-vector rootvech2)])
                               (vector-append i (vector-map (lambda (val) (+ val sizeh1)) j)))
                   (cond ((< (vector-length rootvech1) (vector-length rootvech2))
                          (for/vector ([i (in-vector (vector-take-right rootvech2 (vector-length rootvech1)))])
                                      (vector-map (lambda (val) (+ val sizeh1)) i)))
                         ((> (vector-length rootvech1) (vector-length rootvech2))
                          (vector-take-right rootvech1 (vector-length rootvech2)))
                         (else #())))))
