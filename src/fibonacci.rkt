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
  (cond ((not (heap? h)) (raise-argument-error 'findmin "heap?" h))
        (else (get-value (vector-ref (get-node-vector h) (mcdr (mcar h)))))))

(define (insert! h val)
  (cond ((not (heap? h)) (raise-argument-error 'insert! "heap?" 0 h val))
        ((not (number? val)) (raise-argument-error 'insert! "number?" 1 h val))
        (else
          (let ((rts (get-root-vector h))
                (zerorank (vector-ref (get-root-vector h) 0))
                (node-vector (get-node-vector h)))
            (vector-set! rts 0 (vector-append zerorank (vector (vector-length node-vector))))
            (when (< val (findmin h)) (set-mcdr! (mcar h) (vector-length node-vector)))
            (set-mcar! (mcar h) (vector-append node-vector (vector (make-node val))))))))

;(define (deletemin h)
  ;first make the heap, get min's children and do a meld with remaining elements of heap and children
;  )

(define (meld h1 h2)
  (cond ((not (heap? h1)) (raise-argument-error 'meld "heap?" 0 h1 h2))
        ((not (heap? h2)) (raise-argument-error 'meld "heap?" 1 h1 h2))
        (else
          (let ((rootlenh1 (vector-length (get-root-vector h1)))
                (rootlenh2 (vector-length (get-root-vector h2))))
            (let ((rts (if (<= rootlenh1 rootlenh2) (make-root-vector h2 h1) (make-root-vector h1 h2)))
                  (nodevec (vector-append (get-node-vector h2) (get-node-vector h1)))
                  (minind (if (<= (findmin h1) (findmin h2)) 
                            (get-new-min h1 h2)
                            (get-new-min h2 h1))))
              (mcons (mcons nodevec minind) rts))))))        

