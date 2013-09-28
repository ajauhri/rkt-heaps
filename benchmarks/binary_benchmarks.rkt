#! /usr/bin/env racket
#lang racket
(require plot data/heap racket/cmdline)

(provide plot-graphs)

(plot-new-window? #t)

(define R 4294967087)

(define (make-rand-vector s) 
  (for/vector #:length s ([i (in-range s)]) (random R)))

(define (make-large-heap v)
  (let ((h (make-heap <=)))
   (heap-add-all! h v)
   h))

(define (time-heap-min h)
  (let ((start (current-inexact-milliseconds)) (r (heap-min h)) (end (current-inexact-milliseconds)))
   (- end start))) 

(define (time-heap-remove-min! h)
  (let ((hcopy (heap-copy h)))
   (let ((start (current-inexact-milliseconds)) (r (heap-remove-min! hcopy)) (end (current-inexact-milliseconds)))
    (- end start))))

(define (time-heap-add! h [i (random R)])
  (let ((hcopy (heap-copy h)))
   (let ((start (current-inexact-milliseconds)) (r (heap-add! hcopy i)) (end (current-inexact-milliseconds)))
    (- end start))))

(define (time-heap-add-all! h1 h2)
  (let ((hcopy (heap-copy h1)))
   (let ((start (current-inexact-milliseconds)) (r (heap-add-all! hcopy h2)) (end (current-inexact-milliseconds)))
    (- end start))))

(define (create-timing-vector time-method ssize esize step freq)
  (cond ((eq? time-method time-heap-add-all!)
         (for/vector #:length (/ (- esize ssize) step) ([i (in-range ssize esize step)])
                     (let ((h1 (make-large-heap (make-rand-vector (ceiling (/ (- i 1) 2)))))
                           (h2 (make-large-heap (make-rand-vector (ceiling (/ (- i 1) 2))))))
                       (/ (for/sum ([j (in-range freq)]) (time-method h1 h2)) freq))))
        (else 
          (for/vector #:length (/ (- esize ssize) step) ([i (in-range ssize esize step)])
                      (let ((h (make-large-heap (make-rand-vector (- i 1)))))
                       (/ (for/sum ([j (in-range freq)]) (time-method h)) freq))))))

(define (get-plot-data time-method ssize esize step freq)
  (reverse
    (let ((v (create-timing-vector time-method ssize esize step freq)))
     (for/fold ([lst (list (vector 0 0))]) ([i (in-range ssize esize step)]
                                            [j (in-range (vector-length v))])
               (cons (vector i (vector-ref v j)) lst)))))

(define (plot-graphs ssize esize step freq)
  (define plot-jpeg-quality 0)
  (plot-file (list 
               (lines (get-plot-data time-heap-min ssize esize step freq) #:color 1 #:label "heap-min" #:style 'dot)
               (lines (get-plot-data time-heap-remove-min! ssize esize step freq) #:color 2 #:label "heap-remove-min!" #:style 'solid)
               (lines (get-plot-data time-heap-add! ssize esize step freq) #:color 3 #:label "heap-add!" #:style 'long-dash) 
               (lines (get-plot-data time-heap-add-all! ssize esize step freq) #:color 0 #:label "heap-add-all!" #:style 'dot-dash #:width 2 #:alpha 1)) 
             #:x-label "n" #:y-label "Average time (ms)" (format "binary_~a_~a_~a_~a.pdf" ssize esize step freq) 'pdf))

(command-line 
  #:args
  (ssize esize step freq) (plot-graphs (string->number ssize) (string->number esize) (string->number step) (string->number freq)))

