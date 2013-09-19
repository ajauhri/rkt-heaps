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

(provide makeheap findmin insert! meld)

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
    (let ((rts (get-root-vector h))
          (zerorank (vector-ref (get-root-vector h) 0))
          (node-vector (get-node-vector h)))
      (vector-set! rts 0 (vector-append zerorank (vector (vector-length node-vector))))
      (when (< val (findmin h)) (set-mcdr! (mcar h) (vector-length node-vector)))
      (set-mcar! (mcar h) (vector-append node-vector (vector (make-node val)))))
    #f))

(define (deletemin h)
 ;first make the heap, get min's children and do a meld with remaining elements of heap and children
 ;
  )

(define (meld h1 h2)
  (if (heaps? h1 h2)
    (let ((rootlenh1 (vector-length (get-root-vector h1)))
          (rootlenh2 (vector-length (get-root-vector h2))))
      (if (<= sizeh1 sizeh2)
        (let ((rts (vector-append (get-root-vector h2) (vector-map (lambda (i) 
                                                               (vector-map (lambda (val) (+ val sizeh2)) i))
                                                             (get-root-vector h1))))
              (nodevec (vector-append (get-node-vector h2) (get-node-vector h1)))
              (minind (if (< (findmin h1) (findmin h2)) (+ sizeh2 (get-min-index h1))
                        (get-min-index h2))))
          (mcons (mcons nodevec minind) rts))
        (let ((rts (vector-append (get-root-vector h1) (vector-map (lambda (i) 
                                                               (vector-map (lambda (val) (+ val sizeh1)) i))
                                                             (get-root-vector h2))))
              (nodevec (vector-append (get-node-vector h1) (get-node-vector h2)))
              (minind (if (< (findmin h2) (findmin h1)) (+ sizeh1 (get-min-index h2))
                     (get-min-index h1))))
          (mcons (mcons nodevec minind) rts))))
    #f))
