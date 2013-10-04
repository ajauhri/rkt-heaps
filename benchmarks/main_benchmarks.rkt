#! /usr/bin/env racket
#lang racket
(require plot data/heap "../src/binomial.rkt" "../src/fibonacci.rkt" racket/cmdline)

(provide plot-graphs)

(plot-new-window? #t)

(define R 4294967087)

(define (random-walk h step n)
  (cond ((and (eq? n #f) (>= (vector-length (fi-heap-roots h)) (+ step 1))) (fi-heap-minref h))
        ((>= (vector-length (fi-heap-roots h)) (+ step 1)) n)
        ((eq? n #f) (cond ((>= (random) 0.8) 
                           (random-walk h (+ step 1) (fi-heap-minref h)))
                          (else (let* ((nonemptyrts (for/fold ([res #()])
                                                              ([i (in-range (vector-length (fi-heap-roots h)))])
                                                              (if (> (vector-length (vector-ref (fi-heap-roots h) i)) 0) (vector-append res i) res)))
                                       (ind (vector-ref nonemptyrts (random (vector-length nonemptyrts)))))
                                  (random-walk h (+ step 1) (vector-ref (vector-ref (fi-heap-roots h) ind) 0))))))
        ((eq? (vector-length (fi-node-children n)) 0) (random-walk h (vector-length (fi-heap-roots h)) n))
        (else (let ((child (vector-ref (fi-node-children n) (random (vector-length (fi-node-children n))))))
               (random-walk h (+ step 1) child)))))

(define (make-rand-vector s) 
  (if (= s 0) (vector (random R))
    (for/vector #:length s ([i (in-range s)]) (random R))))

(define (make-large-bi-heap v)
  (let ((h (make-heap <=)))
   (heap-add-all! h v)
   h))

(define (make-large-bino-heap v)
  (for/fold ([h (bino-makeheap (random R))]) ([val (in-vector v)]) (bino-insert h val)))

(define (make-large-scraggly-heap v [count 0] [h (fi-makeheap (random R))])
  (cond ((= count (vector-length v)) h)
        (else (cond ((>= (random) 0.8) (fi-deletemin! h)
                                       (fi-insert! h (vector-ref v count))
                                       (fi-insert! h (random R))
                                       (make-large-scraggly-heap v (+ count 1) h))
                    (else (fi-insert! h (vector-ref v count))
                          (fi-decrement! h (random-walk h 0 #f) (random R))
                          (make-large-scraggly-heap v (+ count 1) h))))))


(define (bi-time-findmin h)
  (let ((start (current-inexact-milliseconds)) (r (heap-min h)) (end (current-inexact-milliseconds)))
   (heap-add! h (random R))
   (- end start)))

(define (bino-time-findmin h)
  (let ((start (current-inexact-milliseconds)) (r (bino-findmin h)) (end (current-inexact-milliseconds)))
   (set! h (bino-insert h (random R)))
   (- end start)))

(define (fi-time-findmin h)
  (let ((start (current-inexact-milliseconds)) (r (fi-findmin h)) (end (current-inexact-milliseconds)))
   (fi-insert! h (random R))
   (- end start)))

(define (bi-time-insert h)
  (let ((i (random R)))
    (let ((start (current-inexact-milliseconds)) (r (heap-add! h i)) (end (current-inexact-milliseconds)))
     (- end start))))

(define (bino-time-insert h)
  (let ((i (random R)))
   (let ((start (current-inexact-milliseconds)) (r (bino-insert h i)) (end (current-inexact-milliseconds)))
    (set! h r)
    (- end start))))

(define (fi-time-insert h)
  (let ((i (random R)))
   (let ((start (current-inexact-milliseconds)) (r (fi-insert! h i)) (end (current-inexact-milliseconds)))
    (- end start))))

(define (bi-time-deletemin h)
    (let ((start (current-inexact-milliseconds)) (r (heap-remove-min! h)) (end (current-inexact-milliseconds)))
     (heap-add! h (random R))
     (heap-add! h (random R))
     (- end start)))

(define (bino-time-deletemin h)
  (let ((start (current-inexact-milliseconds)) (r (bino-deletemin h)) (end (current-inexact-milliseconds)))
   (set! h (bino-meld h (bino-meld (bino-makeheap (random R)) (bino-makeheap (random R)))))
   (- end start)))

(define (fi-time-deletemin h )
  (let ((start (current-inexact-milliseconds)) (r (fi-deletemin! h)) (end (current-inexact-milliseconds)))
   (fi-insert! h (random R))
   (fi-insert! h (random R))
   (- end start))) 

(define (bi-time-meld h1 h2)
  (let ((hcopy (heap-copy h1)))
   (let ((start (current-inexact-milliseconds)) (r (heap-add-all! hcopy h2)) (end (current-inexact-milliseconds)))
    (- end start))))

(define (bino-time-meld h1 h2)
  (let ((start (current-inexact-milliseconds)) (r (bino-meld h1 h2)) (end (current-inexact-milliseconds)))
   (- end start)))

(define (fi-time-meld h1 h2)
  (let ((start (current-inexact-milliseconds)) (r (fi-meld h1 h2)) (end (current-inexact-milliseconds)))
   (- end start)))

(define (time-decrement h [n (random-walk h 0 #f)] [delta (random R)])
  (let ((start (current-inexact-milliseconds)) (r (fi-decrement! h n delta)) (end (current-inexact-milliseconds)))
   (fi-insert! h (random R))
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
        (else 
          (for/vector #:length (/ esize ssize) ([i (in-range ssize (+ esize ssize) ssize)])
                      (let ((h (heap-create-method (make-rand-vector 0))))
                       (/ (for/sum ([j (in-range 0 i)]) 
                                   (time-method h))
                          (* ((car opts) i) i)))))))

(define (logarithmic n) (/ (log n) (log 2)))
(define (linearithmic n) (/ (* (log n) n) (log 2)))
(define (constant n) 1)
(define (linear n) n)

(define (get-plot-data ssize esize
                       heap-create-method 
                       time-method
                       . opts)
  (reverse
    (let ((v (create-timing-vec ssize esize heap-create-method time-method opts)))
     (for/fold ([lst (list (vector 0 0))]) ([i (in-range ssize esize ssize)]
                                            [j (in-range (vector-length v))])
               (cons (vector i (vector-ref v j)) lst)))))

(define (plot-graphs ssize esize)
  (plot-file (list 
               (lines (get-plot-data ssize esize make-large-bi-heap bi-time-insert constant) #:color 4 #:label "binary-insert" #:x-min (+ ssize ssize) #:style 'dot)
               (lines (get-plot-data ssize esize make-large-bino-heap bino-time-insert constant) #:color 1 #:label "binomial-insert" #:x-min (+ ssize ssize) #:style 'short-dash)
               (lines (get-plot-data ssize esize make-large-bino-heap bino-time-insert linear) #:color 3 #:label "binomial-insert (n)" #:x-min (+ ssize ssize) #:style 'long-dash)
               (lines (get-plot-data ssize esize make-large-scraggly-heap fi-time-insert constant) #:color 2 #:label "fibonacci-insert" #:x-min (+ ssize ssize) #:style 'dot-dash))
             #:x-label "n" #:y-label "Average time (ms)/(constant)" (format "insert_~a_~a.pdf" ssize esize) 'pdf)

  (plot-file (list 
               (lines (get-plot-data ssize esize make-large-bi-heap bi-time-deletemin logarithmic) #:color 4 #:label "binary-deletemin" #:x-min (+ ssize ssize) #:style 'dot)
               (lines (get-plot-data ssize esize make-large-bino-heap bino-time-deletemin logarithmic) #:color 1 #:label "binomial-deletemin"  #:x-min (+ ssize ssize) #:style 'short-dash)
               ;(lines (get-plot-data ssize esize make-large-bino-heap time-gen-op bino-deletemin linearithmic) #:color 3 #:label "binomial-deletemin (nlogn)" #:x-min (+ ssize ssize) #:style 'long-dash)
               (lines (get-plot-data ssize esize make-large-scraggly-heap fi-time-deletemin logarithmic) #:color 2 #:label "fibonacci-deletemin"  #:x-min (+ ssize ssize) #:style 'dot-dash))
             #:x-label "n" #:y-label "Average time (ms)/(log n)" (format "deletemin_~a_~a.pdf" ssize esize) 'pdf)

  (plot-file (list 
               (lines (get-plot-data ssize esize make-large-bi-heap bi-time-findmin constant) #:color 4 #:label "binary-findmin" #:x-min (+ ssize ssize) #:style 'dot)
               (lines (get-plot-data ssize esize make-large-bino-heap bino-time-findmin constant) #:color 1 #:label "binomial-findmin"  #:x-min (+ ssize ssize) #:style 'short-dash)
               (lines (get-plot-data ssize esize make-large-scraggly-heap fi-time-findmin constant) #:color 2 #:label "fibonacci-findmin" #:x-min (+ ssize ssize) #:style 'dot-dash))
             #:x-label "n" #:y-label "Average time (ms)/(constant)" (format "findmin_~a_~a.pdf" ssize esize) 'pdf)

  (plot-file (list 
               (lines (get-plot-data ssize esize make-large-bi-heap bi-time-meld logarithmic) #:color 4 #:label "binary-meld"  #:x-min (+ ssize ssize) #:style 'dot)
               (lines (get-plot-data ssize esize make-large-bino-heap bino-time-meld logarithmic) #:color 1 #:label "binomial-meld"  #:x-min (+ ssize ssize) #:style 'short-dash)
               (lines (get-plot-data ssize esize make-large-bino-heap bino-time-meld linearithmic) #:color 3 #:label "binomial-meld (nlogn)" #:x-min (+ ssize ssize) #:style 'long-dash)
               (lines (get-plot-data ssize esize make-large-scraggly-heap fi-time-meld logarithmic) #:color 2 #:label "fibonacci-meld"  #:x-min (+ ssize ssize) #:style 'dot-dash))
             #:x-label "n" #:y-label "Average time (ms)/(log n)" (format "meld_~a_~a.pdf" ssize esize) 'pdf)

  (plot-file (list 
               (lines (get-plot-data ssize esize make-large-scraggly-heap time-decrement constant) #:color 2 #:label "fibonacci-decrement" #:x-min (+ ssize ssize) #:style 'dot-dash))
             #:x-label "n" #:y-label "Average time (ms)/(log n)" (format "decrement_~a_~a.pdf" ssize esize) 'pdf)


  (plot-file (list 
               (lines (get-plot-data ssize esize make-large-scraggly-heap time-delete logarithmic) #:color 2 #:label "fibonacci-delete"  #:x-min (+ ssize ssize) #:style 'dot-dash))
             #:x-label "n" #:y-label "Average time (ms)/(log n)" (format "delete_~a_~a.pdf" ssize esize) 'pdf))


(command-line 
  #:args
  (ssize esize) (plot-graphs (string->number ssize) (string->number esize)))
