#lang racket

(require rackunit "../src/helper.rkt" "../src/binomial.rkt")

(run-test
  (test-suite
    "binomial helper functions binomail heap"

    (test-case
      "Checking insert"
      (check-true (heaps? (insert (makeheap 1) 2))))

    (test-case
      "Checking heaps? and heap? works fine"
      (check-true (heaps? (makeheap 1))) 
      (check-false (heap? (cons (cons (vector #f 1 1) 2) 0)))
      (check-true (heaps? (makeheap 1) (makeheap 2) (makeheap 3))))))

