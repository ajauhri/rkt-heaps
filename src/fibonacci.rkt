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

(provide makeheap findmin insert deletemin meld decrement! (struct-out heap) (struct-out node))

(define (makeheap val)
  (let ((n (node val #f #() #f)))
   (heap n (vector (vector n)) 1)))

(define (findmin h)
  (cond ((not (heap? h)) (raise-argument-error 'findmin "heap?" h))
        ((= (heap-size h) 0) (raise-type-error 'findmin "node" (heap-minref h)))
        (else (node-val (heap-minref h)))))

(define (insert h val)
  (cond ((not (heap? h)) (raise-argument-error 'insert "heap?" 0 h val))
        ((not (number? val)) (raise-argument-error 'insert "number?" 1 h val))
        (else
          (meld h (makeheap val)))))

; Commentary:
; - this procedure is a mutator since it changes the node->parent & node->children of the previously defined nodes in the heap
; - does not edit the heap structure passed as an argument
(define (deletemin h)
  (cond ((not (heap? h)) (raise-argument-error 'deletemin "heap?" h))
        ((= (heap-size h) 0) (raise-type-error 'deletemin "node" (heap-minref h)))
        (else 
          (let* ((maxrnk (inexact->exact (ceiling (/ (log (heap-size h)) (log 2)))))

                 ; put children of min into a root vector
                 (newrts (create-children-rts-vec (node-children (heap-minref h)) maxrnk))
                 (oldrts (heap-roots h)))

            ; combine other roots into the ^root vector 
            (for ([i (in-range (vector-length newrts))])
                 (let ((ithrankvec1 (vec-ref oldrts i))
                       (ithrankvec2 (vec-ref newrts i)))
                   (vector-set! newrts i (vector-append ithrankvec1 ithrankvec2))))

            ; certain no two roots of same rank in the rank vector, & also remove min root
            (for ([i (in-range (vector-length newrts))])
                 (let ((ithrankvec (vector-ref newrts i))
                       (minnode (heap-minref h)))
                   (when (> (vector-length ithrankvec) 1) (correct-rts-vec! newrts i minnode))))

            ; find min
            (let ((minref (for/fold ([m #f])
                                    ([i (in-vector newrts)])
                                    (values (cond ((and (node? m) (> (vector-length i) 0)) 
                                                   (if (< (node-val (vector-ref i 0)) (node-val m)) (vector-ref i 0) m))
                                                  ((> (vector-length i) 0) (vector-ref i 0)))))))
              (heap minref newrts (- (heap-size h) 1)))))))


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
                (minref (if (> (findmin h1) (findmin h2))
                          (heap-minref h2)
                          (heap-minref h1))))
            (heap minref rts (+ (heap-size h1) (heap-size h2)))))))


(define (decrement! h noderef delta)
  (cond ((not (heap? h)) (raise-argument-error 'decrement "heap?" 0 h noderef delta))
        ((not (node? noderef)) (raise-argument-error 'decrement "node?" 1 h noderef delta))
        ((not (number? delta)) (raise-argument-error 'decrement "number?" 2 h noderef delta))
        (else (set-node-val! noderef (- (node-val noderef) delta))
              (when (< (node-val noderef) (findmin h)) (set-heap-minref! h noderef))
              
              ; heap condition violated
              (cond ((and (not (eq? (node-parent noderef) #f)) (< (node-val noderef) (node-val (node-parent noderef))))
                     (let ((nodernk (vector-length (node-children noderef)))
                           (parent (node-parent noderef)))
                       (set-node-parent! noderef #f)
                       (set-node-marked! noderef #f)
                       (vector-set! (heap-roots h) nodernk (vector-append (vector-ref (heap-roots h) nodernk) (vector noderef)))
                       (check-parents! h parent noderef))))))) 

(define (delete! h noderef)
  (cond ((not (heap? h))) (raise-argument-error 'delete! "heap?" 0 h noderef)
        ((not (node? noderef)) (raise-argument-error 'delete! "node?" 1 h noderef))
        
        ; add all children nodes of the node to be deleted
        (else (for ([i (in-vector (node-children noderef))])
                   (let* ((noderank (vector-length (node-children i))))
                    (vector-set! (heap-roots h) noderank 
                                 (vector-append (vector-ref (heap-roots h) noderank) (vector i)))))
              
              ; if node to be deleted is the min node 
              (cond ((eq? noderef (heap-minref h)) (deletemin h)))
             
              ; if parent does not exist, remove the node from set of roots
              (cond ((eq? #f (node-parent noderef))
                     (vector-set! (heap-roots h) (vector-length (node-children noderef)) 
                                  (for/vector ([i (in-vector (vector-ref (heap-roots h) (vector-length (node-children noderef))))]) 
                                              (if (eq? i noderef) #() i))))
                    
                    ; if parent exists, recursively remove parents if marked 
                    ((not (eq? #f (node-parent noderef))) 
                     (check-parents! h (node-parent noderef) noderef))))))
