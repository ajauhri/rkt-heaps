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
    (let ((rts (get-roots h))
          (zerorank (vector-ref (get-roots h) 0))
          (node-vector (get-node-vector h)))
      (vector-set! rts 0 (vector-append zerorank (vector (vector-length node-vector))))
      (when (< val (findmin h)) (set-mcdr! (mcar h) (vector-length node-vector)))
      (set-mcar! (mcar h) (vector-append node-vector (vector (make-node val)))))
    #f))

(define (meld h1 h2)
  (if (heaps? h1 h2)
    (let ((sizeh1 (vector-length (get-node-vector h1)))
          (sizeh2 (vector-length (get-node-vector h2))))
      (if (<= sizeh1 sizeh2)
        (let ((rts (vector-append (get-roots h2) (vector-map (lambda (i) 
                                                               (vector-map (lambda (val) (+ val sizeh2)) i))
                                                             (get-roots h1))))
              (nodevec (vector-append (get-node-vector h2) (get-node-vector h1)))
              (minind (if (< (findmin h1) (findmin h2)) (+ sizeh2 (get-min-index h1))
                        (get-min-index h2))))
          (mcons (mcons nodevec minind) rts))
        (let ((rts (vector-append (get-roots h1) (vector-map (lambda (i) 
                                                               (vector-map (lambda (val) (+ val sizeh1)) i))
                                                             (get-roots h2))))
              (nodevec (vector-append (get-node-vector h1) (get-node-vector h2)))
              (minind (if (< (findmin h2) (findmin h1)) (+ sizeh1 (get-min-index h2))
                     (get-min-index h1))))
          (mcons (mcons nodevec minind) rts))))
    #f))
