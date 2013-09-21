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

(provide makeheap findmin insert deletemin meld heap? heap-roots heap-minind heap-size)

(define (makeheap val)
  (let ((n (node val #f #() #f)))
   (heap n (vector (vector n)) 1)))

(define (findmin h)
  (cond ((not (heap? h)) (raise-argument-error 'findmin "heap?" h))
        ((= (heap-size h) 0) (raise-type-error 'findmin "node" (heap-minind h)))
        (else (node-val (heap-minind h)))))

(define (insert h val)
  (cond ((not (heap? h)) (raise-argument-error 'insert! "heap?" 0 h val))
        (else
          (meld h (makeheap val)))))

(define (deletemin h)
  (cond ((not (heap? h)) (raise-argument-error 'deletemin "heap?" h))
        ((= (heap-size h) 0) (raise-type-error 'deletemin "node" (heap-minind h)))
        (else 
          (let* ((maxrnk (inexact->exact (ceiling (/ (log (heap-size h)) (log 2)))))

                 ; put children of min into a root vector
                 (newrts (create-rts-vec (node-children (heap-minind h)) maxrnk))
                 (oldrts (heap-roots h)))

            ; combine other roots into the ^root vector 
            (for ([i (in-range (vector-length newrts))])
                 (let ((ithrankvec1 (vec-ref oldrts i))
                       (ithrankvec2 (vec-ref newrts i)))
                   (vector-set! newrts i (vector-append ithrankvec1 ithrankvec2))))

            ; certain no two roots of same rank in the rank vector, & also remove min root
            (for ([i (in-range (vector-length newrts))])
                 (let ((ithrankvec (vector-ref newrts i))
                       (minnode (heap-minind h)))
                  (when (> (vector-length ithrankvec) 1) (correct-rts-vec! newrts i minnode))))

            ; find min
            (let ((min (for/fold ([m #f])
                                 ([i (in-vector newrts)])
                                 (values (cond ((and (node? m) (> (vector-length i) 0)) 
                                                (if (< (node-val (vector-ref i 0)) (node-val m)) (vector-ref i 0) m))
                                               ((> (vector-length i) 0) (vector-ref i 0)))))))
              (heap min newrts (- (heap-size h) 1)))))))

(define (meld h1 h2)
  (cond ((not (heap? h1)) (raise-argument-error 'meld "heap?" 0 h1 h2))
        ((not (heap? h2)) (raise-argument-error 'meld "heap?" 1 h1 h2))
        (else
          (let ((rts (vector-append (for/vector ([i (in-vector (heap-roots h1))]
                                                 [j (in-vector (heap-roots h2))])
                                                (vector-append i j))
                                    (cond ((< (vector-length (heap-roots h1)) (vector-length (heap-roots h2)))
                                           (vector-take-right (heap-roots h2) (vector-length (heap-roots h1))))
                                          ((> (vector-length (heap-roots h1)) (vector-length (heap-roots h2)))
                                           (vector-take-right (heap-roots h1) (vector-length (heap-roots h2))))
                                          (else #()))))
                (min (if (> (findmin h1) (findmin h2))
                       (heap-minind h2)
                       (heap-minind h1))))
            (heap min rts (+ (heap-size h1) (heap-size h2)))))))

