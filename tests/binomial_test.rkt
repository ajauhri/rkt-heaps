#lang racket
(require rackunit "../src/binomial.rkt")
(run-test
  (test-suite
    "binomial heap"
    (test-case
      "Checking makeheap"
      (check-true (equal? (cons (cons (vector 1) 1) 0) (makeheap 1))))

    (test-case
      "Checking findmin"
      (check-eq? 1 (findmin (makeheap 1))) 
      (let ((h (insert (insert (makeheap 3) 4) 2))) (check-eq? 2 (findmin h))))

    (test-case
      "Checking insert"
      (check-true (equal? (insert (makeheap 1) 2) (meld (makeheap 1) (makeheap 2)))))

    (test-case
      "Checking deletemin"
      (check-false (deletemin (cons (cons (vector 2 3 4) 3) 2))))

    (test-case
      "Checking meld for not valid heaps"
      (check-false (meld (makeheap 1) (cons (cons #(1 2 3) 3) 2)))
      (check-false (meld (makeheap 1) (cons (cons '(1 2 3) 3) 0)))
      (check-true (equal? (cons (cons (vector #f 1 1) 2) 1) (meld (makeheap 1) (makeheap 1)))))))

