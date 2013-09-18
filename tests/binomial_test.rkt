#lang racket
(require rackunit "../src/binomial.rkt")
(run-test
  (test-suite
    "binomial heap"
    (test-case
      "Checking makeheap creates a heap with the correct structure"
      (check-true (equal? (cons (cons (vector 1) 1) 1) (makeheap 1))))

    (test-case
      "Checking findmin with just one value"
      (check-eq? 1 (findmin (makeheap 1))))

    (test-case
      "checking findmin with a bigger set of values"
      (let ((h (insert (insert (makeheap 2) 3) 4))) (check-eq? 2 (findmin h))))

    (test-case
      "checking findmin to return #f if min pointer not provided"
      (check-false (findmin (cons (cons (vector 1 2) 2) #f))))

    (test-case
      "Checking insert with valid heap"
      (check-true (equal? (insert (makeheap 1) 2) (meld (makeheap 1) (makeheap 2)))))

    (test-case 
      "checking deletemin with 2^n elements & since there are only 2 elements, the resultant sub-heap will be empty"
      (check-equal? (cons (cons (vector 2) 1) 2) (deletemin (meld (makeheap 1) (makeheap 2)))))

    (test-case 
      "Checking deletemin with min element being the only element of its tree"
      (check-equal? (cons (cons (vector #f 2 3) 2) 2) (deletemin (insert (meld (makeheap 2) (makeheap 3)) 1))))

    (test-case
      "Checking meld for wrong min value. Since, heap-lazy? is applied here, this will pass"
      (check-equal? (cons (cons (vector #f #f #f 1 1 2 3) 4) 1) (meld (makeheap 1) (cons (cons #(1 2 3) 3) 2))))

    (test-case
      "Checking a simple meld"
      (check-true (equal? (cons (cons (vector #f 1 1) 2) 1) (meld (makeheap 1) (makeheap 1))))))) 

