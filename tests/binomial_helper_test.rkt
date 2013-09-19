#lang racket

(require rackunit "../src/binomial_helper.rkt" "../src/binomial.rkt")

(run-test
  (test-suite
    "binomial helper functions binomail heap"

    (test-case
      "Checking insert"
      (check-true (heap? (insert (makeheap 1) 2))))

    (test-case
      "Checking heaps? and heap? works fine"
      (check-true (heap-lazy? (cons (cons (vector 1 1) 2) #f))) 
      (check-false (heap? (cons (cons (vector #f 1 1) 2) 0))))))

