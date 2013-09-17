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
      "Checking insert with invalid heap"
      (check-false (insert (cons (cons (list 1 2 3) 3) 0) 1)))
    
    (test-case
      "Checking insert with valid heap"
      (check-true (equal? (insert (makeheap 1) 2) (meld (makeheap 1) (makeheap 2)))))

    (test-case
      "Checking deletemin with wrong min value"
      (check-false (deletemin (cons (cons (vector 2 3 4) 3) 3))))
    
    (test-case 
      "checking deletemin with 2^n elements"
      (check-equal? (cons (cons (vector 2) 1) 2) (deletemin (meld (makeheap 1) (makeheap 2)))))
    
    (test-case
      "Checking meld for not valid heaps"
      (check-false (meld (makeheap 1) (cons (cons #(1 2 3) 3) 2)))
      (check-false (meld (makeheap 1) (cons (cons '(1 2 3) 3) 0)))
      (check-true (equal? (cons (cons (vector #f 1 1) 2) 1) (meld (makeheap 1) (makeheap 1)))))))

