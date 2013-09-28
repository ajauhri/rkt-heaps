#! /usr/bin/env racket
#lang racket
(require rackunit "../src/fibonacci.rkt" "../src/fibonacci_helper.rkt" rackunit/text-ui)
(run-tests
  (test-suite
    "fibonacci heap"
    (test-case
      "Checking makeheap creates a opaque structure"
      (define n (node 1 #f #() #f))
      (check-false (equal? (heap n (vector (vector n)) 1) (makeheap 1))))

    (test-case
      "Checking findmin with one value"
      (check-eq? 1 (findmin (makeheap 1))))

    (test-case
      "checking findmin, decrement! and delete!" 
      (let ((h (meld (makeheap 1) (makeheap 2)))) 
       (insert! h 3)
       (insert! h 4)
       (insert! h 5)
       (deletemin! h)
       (define n (vector-ref (node-children (vector-ref (vector-ref (heap-roots h) 2) 0)) 0))
       (decrement! h n 2) 
       (delete! h n)
       (check-eq? 2 (findmin h))))

    (test-case
      "parent gets marked when one of its children are deleted"
      (let ((h (meld (makeheap 1) (makeheap 2))))
       (insert! h 3)
       (insert! h 4)
       (insert! h 5)
       (deletemin! h)
       (define n (vector-ref (vector-ref (heap-roots h) 2) 0))
       (define c1 (vector-ref (node-children n) 1))
       (define c2 (vector-ref (node-children c1) 0))
       (delete! h c2)
       (check-true (node-marked c1))))

    (test-case
      "If heap property is violated during decrement, parent's vector of children is updated correctly"
      (let ((h (meld (makeheap 1) (makeheap 2))))
       (insert! h 3)
       (insert! h 4)
       (insert! h 5)
       (deletemin! h)
       (define n (vector-ref (vector-ref (heap-roots h) 2) 0))
       (define c1 (vector-ref (node-children n) 1))
       (define c2 (vector-ref (node-children c1) 0))
       (decrement! h c2 4)
       (check-true (eq? (vector-length (node-children c1)) 0))))

    (test-case
      "If two children of a root node are deleted, it shouldn't be marked"
      (let ((h (meld (makeheap 1) (makeheap 2))))
       (insert! h 3)
       (insert! h 4)
       (insert! h 5)
       (deletemin! h)
       (define n (vector-ref (vector-ref (heap-roots h) 2) 0))
       (define c1 (vector-ref (node-children n) 0))
       (define c2 (vector-ref (node-children n) 1))
       (delete! h c1)
       (delete! h c2)
       (check-false (node-marked n))
       (deletemin! h)
       (check-eq? 5 (findmin h))))))

