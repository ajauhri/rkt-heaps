#lang racket 
;; Book-keeper fof the functions to be implemented 
;; makeheap
;; findmin
;; insert
;; deletemin
;; meld
;; decrement
;; delete
(require racket/mpair "fibonacci_helper.rkt" compatibility/mlist)

(provide makeheap findmin insert!)

(define (makeheap val)
  (let ((rts (vector (vector 0))))
   (mcons 
    (mcons (vector (make-node val)) 0) 
    rts)))

(define (findmin h)
  (if (heap? h)
    (get-value (vector-ref (get-node-vector h) (mcdr (mcar h))))
    #f))

(define (insert! h val)
  (if (heap? h)
    (let ((rts (get-roots h))
          (zerorank (vector-ref (get-roots h) 0))
          (node-vector (get-node-vector h)))
     (vector-set! rts 0 (vector-append zerorank (vector (vector-length node-vector))))
     (when (< val (findmin h)) (set-mcdr! (mcar h) (vector-length node-vector)))
     (set-mcar! (mcar h) (vector-append node-vector (vector (make-node val)))))
    #f))

(define (meld h1 h2)
  (if (heaps? h1 h2)
    (let ((rts (if (< (vector-length (get-roots h1)) (vector-length (get-roots h2))
                      (vector-append (vector-map (lambda (i) (+ v))))
                      )))))
    ) 
  )
