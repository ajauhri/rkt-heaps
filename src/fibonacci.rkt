#lang racket 
;; Book-keeper fof the functions to be implemented 
;; makeheap
;; findmin
;; insert
;; deletemin
;; meld
;; decrement
;; delete
(require "fibonacci_helper.rkt")

(provide makeheap findmin insert meld)

(define (makeheap val)
  (let ((rts (vector (vector 0))))
   (cons (cons (vector (make-node val)) 0) 
         rts)))

(define (findmin h)
  (cond ((not (heap? h)) (raise-argument-error 'findmin "heap?" h))
        (else (get-value (vector-ref (get-node-vector h) (cdr (car h)))))))

(define (insert h val)
  (cond ((not (heap? h)) (raise-argument-error 'insert! "heap?" 0 h val))
        ((not (number? val)) (raise-argument-error 'insert! "number?" 1 h val))
        (else
          (meld h (makeheap val)))))

;(define (deletemin h)
;first make the heap, get min's children and do a meld with remaining elements of heap and children
;  )

(define (meld h1 h2)
  (cond ((not (heap? h1)) (raise-argument-error 'meld "heap?" 0 h1 h2))
        ((not (heap? h2)) (raise-argument-error 'meld "heap?" 1 h1 h2))
        (else
          (let ((rts (make-root-vector h1 h2))
                (nodevec (vector-append (get-node-vector h1) (get-node-vector h2)))
                (minind (if (> (findmin h1) (findmin h2)) 
                          (+ (vector-length (get-node-vector h1)) (get-min-index h2))
                          (get-min-index h1))))
            (cons (cons nodevec minind) rts)))))

