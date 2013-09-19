#lang racket

(require racket/mpair)
(provide make-node get-root-vector get-node-vector get-value get-min-index heap? make-root-vector get-new-min)

(define (make-node val)
  (vector #f val #f (vector)))

(define (get-root-vector h)
  (mcdr h))

(define (get-node-vector h)
  (mcar (mcar h)))

(define (get-value n)
  (vector-ref n 1))

(define (get-min-index h)
  (mcdr (mcar h)))

(define (heap? h) #t)

; pre-condition: length of the root vector for h2 should be small or equal to the length of root vector for h1
(define (make-root-vector h1 h2 )
  (let ((rootvech1 (get-root-vector h1))
        (rootvech2 (get-root-vector h2))
        (sizeh1 (vector-length (get-node-vector h1))))
    (if (< (vector-length rootvech1) (vector-length rootvech2))
      (raise-argument-error 'make-root-vector "(< (vector-length rootvech1) (vector-length rootvech2)" h1)
      (for/vector #:length (vector-length rootvech2) ([i (in-vector rootvech1)]
                                                      [j (in-vector rootvech2)])
                  (vector-append i (vector-map (lambda (val) (+ val sizeh1)) j))))))

; pre-condition: (<= (findmin h1) (findmin h2))
(define (get-new-min h1 h2)
  (if (> (get-value (vector-ref (get-node-vector h1) (mcdr (mcar h1)))) 
         (get-value (vector-ref (get-node-vector h2) (mcdr (mcar h2)))))
    (raise-argument-error 'get-min-index "(> (findmin h1) (findmin h2))" h1)
    (if (< (vector-length (get-root-vector h1)) (vector-length (get-root-vector h2)))
      (+ (vector-length (get-node-vector h2)) (get-min-index h1))
      (get-min-index h1))))

