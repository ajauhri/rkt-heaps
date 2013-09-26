#lang racket
(require plot "../src/fibonacci.rkt")
(plot-new-window? #t)

(provide plot-graphs)

(define R 4294967087)

(define (random-walk h step n)
  (cond ((and (eq? n #f) (>= (vector-length (heap-roots h)) (+ step 1))) (heap-minref h))
        ((>= (vector-length (heap-roots h)) (+ step 1)) n)
        ((eq? n #f) (cond ((>= (random) 0.8) 
                           (random-walk h (+ step 1) (heap-minref h)))
                          (else (let* ((nonemptyrts (for/fold ([res #()])
                                                              ([i (in-range (vector-length (heap-roots h)))])
                                                              (if (> (vector-length (vector-ref (heap-roots h) i)) 0) (vector-append res i) res)))
                                       (ind (vector-ref nonemptyrts (random (vector-length nonemptyrts)))))
                                  (random-walk h (+ step 1) (vector-ref (vector-ref (heap-roots h) ind) 0))))))
        ((eq? (vector-length (node-children n)) 0) (random-walk h (vector-length (heap-roots h)) n))
        (else (let ((child (vector-ref (node-children n) (random (vector-length (node-children n))))))
               (random-walk h (+ step 1) child)))))

(define (make-rand-vector s) 
  (for/vector #:length s ([i (in-range s)]) (random R)))

;(define (make-large-heap v [h (makeheap (random R))])
;  (for ([val (in-vector v)]) (insert! h val))
;  h)

(define (make-large-scraggly-heap v [count 0] [h (makeheap (random R))])
  (cond ((= count (vector-length v)) h)
        (else (cond ((>= (random) 0.8) (deletemin! h)
                                       (insert! h (vector-ref v count))
                                       (insert! h (random R))
                                       (make-large-scraggly-heap v (+ count 1) h))
                    (else (insert! h (vector-ref v count))
                          (make-large-scraggly-heap v (+ count 1) h))))))

(define (time-findmin h)
  (let ((start (current-inexact-milliseconds)) (r (findmin h)) (end (current-inexact-milliseconds)))
   (- end start))) 

(define (time-deletemin h)
  (let ((start (current-inexact-milliseconds)) (r (deletemin! h)) (end (current-inexact-milliseconds)))
   (insert! h (random R))
   (- end start)))

(define (time-insert h [i (random R)])
  (let ((start (current-inexact-milliseconds)) (r (insert! h i)) (end (current-inexact-milliseconds)))
   (deletemin! h)
   (- end start)))

(define (time-meld h1 h2)
  (let ((start (current-inexact-milliseconds)) (r (meld h1 h2)) (end (current-inexact-milliseconds)))
   (- end start)))

(define (time-decrement h [n (random-walk h 0 #f)] [delta (random R)])
  (let ((start (current-inexact-milliseconds)) (r (decrement! h n delta)) (end (current-inexact-milliseconds)))
   (- end start)))

(define (time-delete h [n (random-walk h 0 #f)])
  (let ((start (current-inexact-milliseconds)) (r (delete! h n)) (end (current-inexact-milliseconds)))
   (insert! h (random R))
   (- end start)))

(define (create-timing-vector time-method ssize esize step freq)
  (cond ((eq? time-method time-meld)
         (for/vector #:length (/ (- esize ssize) step) ([i (in-range ssize esize step)])
                     (let ((h1 (make-large-scraggly-heap (make-rand-vector (ceiling (/ (- i 1) 2)))))
                           (h2 (make-large-scraggly-heap (make-rand-vector (ceiling (/ (- i 1) 2))))))
                       (/ (for/sum ([j (in-range freq)]) (time-method h1 h2)) freq))))
        (else 
          (for/vector #:length (/ (- esize ssize) step) ([i (in-range ssize esize step)])
                      (let ((h (make-large-scraggly-heap (make-rand-vector (- i 1)))))
                       (/ (for/sum ([j (in-range freq)]) (time-method h)) freq))))))

(define (get-plot-data time-method ssize esize step freq)
  (reverse
    (let ((v (create-timing-vector time-method ssize esize step freq)))
     (for/fold ([lst (list (vector 0 0))]) ([i (in-range ssize esize step)]
                                            [j (in-range (vector-length v))])
               (cons (vector i (vector-ref v j)) lst)))))

(define (plot-graphs ssize esize step freq)
  (plot (list 
          (lines (get-plot-data time-findmin ssize esize step freq) #:color 1 #:label "findmin")
          (lines (get-plot-data time-deletemin ssize esize step freq) #:color 2 #:label "deletemin")
          (lines (get-plot-data time-insert ssize esize step freq) #:color 3 #:label "insert")
          (lines (get-plot-data time-meld ssize esize step freq) #:color 0 #:label "meld")
          (lines (get-plot-data time-decrement ssize esize step freq) #:color 4 #:label "decrement")
          (lines (get-plot-data time-delete ssize esize step freq) #:color 4 #:label "delete"))
        #:x-label "n" #:y-label "Average time (ms)" #:out-file (format "fibonacci_~a_~a_~a_~a" ssize esize step freq) #:out-kind 'png))
