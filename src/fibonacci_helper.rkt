#lang racket

(provide create-rts-vec correct-rts-vec! vec-ref (struct-out heap) (struct-out node))

(struct heap (minind roots size))
(struct node (val parent children marked) #:mutable)
 
(define (create-rts-vec nodevec maxrnk)
  (cond ((not (exact-nonnegative-integer? maxrnk)) (raise-argument-error 'create-rts-vec "exact" maxrnk))
        ((= maxrnk 0) #())
        (else (let  ((res (make-vector maxrnk #())))
               (for ([i (in-range (vector-length nodevec))])
                    (let* ((node (vector-ref nodevec i))
                           (noderank (vector-length (node-children node))))
                      (vector-set! res noderank 
                                   (vector-append (vector-ref res noderank) (vector node)))))
               res))))


(define (correct-rts-vec! rts i minnode)
  (cond ((<= (vector-length (vector-ref rts i)) 1) '())
        (else (let* ((subelems (vector-take (vector-ref rts i) 2))
                     (node1 (vector-ref subelems 0))
                     (node2 (vector-ref subelems 1))
                     (foo (vector-set! rts i (vector-drop (vector-ref rts i) 2))))
                (cond ((eq? node1 minnode) (vector-set! rts i (vector-append (vector node2) (vector-ref rts i))))
                      ((eq? node2 minnode) (vector-set! rts i (vector-append (vector node1) (vector-ref rts i))))
                      ((<= (node-val node1) (node-val node2))
                       (set-node-parent! node2 node1)
                       (set-node-children! node1 (vector-append (node-children node1) (vector node2)))
                       (vector-set! rts (+ i 1) (vector-append (vector-ref rts (+ i 1)) (vector node1))))
                      ((> (node-val node1) (node-val node2))
                       (set-node-parent! node1 node2)
                       (set-node-children! node2 (vector-append (node-children node2) (vector node1)))
                       (vector-set! rts (+ i 1) (vector-append (vector-ref rts (+ i 1)) (vector node2)))))
                (correct-rts-vec! rts i minnode)))))

(define (vec-ref vec pos)
  (cond ((<= (vector-length vec) pos) #())
        (else (vector-ref vec pos))))
