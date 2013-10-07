#! /usr/bin/env racket
#lang racket
(require plot data/heap "../src/binomial.rkt" "../src/fibonacci.rkt" racket/cmdline)

(provide random-walk make-rand-vector make-bi-heap make-bino-heap make-fi-heap get-plot-data logarithmic linear constant linearithmic)

(define R 4294967087)

(define (fib-seq vec cur next remaining)
  (if (= 0 remaining)
    vec
    (fib-seq (vector-append vec (vector next)) next (+ cur next) (- remaining 1))))

(define (random-walk h ri)
  (let* ((root (for/fold ([n (fi-heap-minref h)])
                        ([i (in-range ri)])
                        (fi-node-right n)))
        (maxrnk (inexact->exact (ceiling (/ (log (fi-heap-size h)) (log 2)))))
        (fibvec (fib-seq (vector 0) 0 1 (+ 1 maxrnk))) ;+1 may turn out to be a hack
        (depth (vector-ref fibvec (+ ri 1)))) ; max depth of a path is F_[d+2], where F_i is the ith fibonacci number. The indicies adjustment makes it (ri+1)
   
    (define (random-depth-traversal n step depth)
      (cond ((= step depth) n)
            ((> (vector-length (fi-node-children n)) 0) 
             (let ((childind (random (vector-length (fi-node-children n)))))
              (random-depth-traversal (vector-ref (fi-node-children n) childind) (+ step 1) depth)))
            (else n)))
    
    (random-depth-traversal root 1 depth)))

(define (make-rand-vector s) 
  (if (= s 0) (vector (random R))
    (for/vector #:length s ([i (in-range s)]) (random R))))

(define (make-bi-heap v)
  (let ((h (make-heap <=)))
   (heap-add-all! h v)
   h))

(define (make-bino-heap v)
  (for/fold ([h (bino-makeheap (random R))]) ([val (in-vector v)]) (bino-insert h val)))

(define (make-fi-heap v [count 0] [h (fi-makeheap (random R))])
  (cond ((= count (vector-length v)) h)
        (else (cond ((>= (random) 0.8) (fi-deletemin! h)
                                       (set! h (fi-insert! h (vector-ref v count)))
                                       (set! h (fi-insert! h (random R)))
                                       (make-fi-heap v (+ count 1) h))
                    (else (set! h (fi-insert! h (vector-ref v count)))
                          (fi-decrement! h (random-walk h (random (+ 1 (inexact->exact (ceiling (/ (log (fi-heap-size h)) (log 2))))))) (random R))
                          (make-fi-heap v (+ count 1) h))))))

(define (bi-time-meld h1 h2)
  (let ((hcopy (heap-copy h1)))
   (let ((start (current-inexact-milliseconds)) (r (heap-add-all! hcopy h2)) (end (current-inexact-milliseconds)))
    (- end start))))

(define (bino-time-meld h1 h2)
  (let ((start (current-inexact-milliseconds)) (r (bino-meld h1 h2)) (end (current-inexact-milliseconds)))
   (- end start)))

(define (fi-time-meld h1 h2)
  (let ((start (current-inexact-milliseconds)) (r (fi-meld! h1 h2)) (end (current-inexact-milliseconds)))
   (- end start)))

(define (time-decrement h [n (random-walk h 0 #f)] [delta (random R)])
  (let ((start (current-inexact-milliseconds)) (r (fi-decrement! h n delta)) (end (current-inexact-milliseconds)))
   (- end start)))

(define (time-delete h [n (random-walk h 0 #f)])
  (let ((start (current-inexact-milliseconds)) (r (fi-delete! h n)) (end (current-inexact-milliseconds)))
   (fi-insert! h (random R))
   (fi-insert! h (random R))
   (- end start)))


(define (create-timing-vec ssize esize heap-create-method time-method opts)
  (cond ((or (eq? time-method bi-time-meld) (eq? time-method bino-time-meld) (eq? time-method fi-time-meld))
         (for/vector #:length (/ esize ssize) ([i (in-range ssize (+ esize ssize) ssize)])
                     (/ (for/sum ([size (in-range 2 i)]) 
                                 (let ((h1 (heap-create-method (make-rand-vector (ceiling (/ size 2)))))
                                       (h2 (heap-create-method (make-rand-vector (ceiling (/ (- size 1) 2))))))
                                   (time-method h1 h2))) (* ((car opts) i) i))))
        ((eq? time-method time-decrement)
         (for/vector #:length (/ esize ssize) ([i (in-range ssize (+ esize ssize) ssize)])
                     (/ (for/sum ([j (in-range 1 (+ i 1))]) 
                                 (let ((h (heap-create-method (make-rand-vector j))))
                                  (time-method h)))
                        (* ((car opts) i) i))))
        ((eq? time-method time-delete)
         (for/vector #:length (/ esize ssize) ([i (in-range ssize (+ esize ssize) ssize)])
                     (/ (for/sum ([j (in-range 1 (+ i 1))]) 
                                 (let ((h (heap-create-method (make-rand-vector j))))
                                  (time-method h))))))
        (else 
          (for/vector #:length (/ esize ssize) ([i (in-range ssize (+ esize ssize) ssize)])
                      
                      (/ (for/sum ([j (in-range 1 (+ i 1))]) 
                                  (let ((h (heap-create-method (make-rand-vector j))))
                                   (time-method h)))
                         (* ((car opts) i) i))))))

(define (logarithmic n) (/ (log n) (log 2)))
(define (linearithmic n) (/ (* (log n) n) (log 2)))
(define (constant n) 1)
(define (linear n) n)

(define (get-plot-data ssize esize
                       timing-vec-op
                       heap-create-method 
                       time-method
                       . opts)
  (reverse
    (let ((v (timing-vec-op ssize esize heap-create-method time-method opts)))
     (for/fold ([lst (list (vector 0 0))]) ([i (in-range ssize (+ esize ssize) ssize)]
                                            [j (in-range (vector-length v))])
               (cons (vector i (vector-ref v j)) lst)))))

(define (plot-graphs ssize esize)
  
  (plot-file (list 
               (lines (get-plot-data ssize esize make-fi-heap time-decrement constant) #:color 2 #:label "fibonacci-decrement" #:x-min (+ ssize ssize) #:style 'dot-dash))
             #:x-label "n" #:y-label "Average time (ms)/(log n)" (format "decrement_~a_~a.pdf" ssize esize) 'pdf)


  (plot-file (list 
               (lines (get-plot-data ssize esize make-fi-heap time-delete logarithmic) #:color 2 #:label "fibonacci-delete"  #:x-min (+ ssize ssize) #:style 'dot-dash))
             #:x-label "n" #:y-label "Average time (ms)/(log n)" (format "delete_~a_~a.pdf" ssize esize) 'pdf))

