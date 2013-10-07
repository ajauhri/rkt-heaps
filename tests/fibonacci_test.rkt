#! /usr/bin/env racket
#lang racket
(require rackunit "../src/fibonacci.rkt" "../src/fibonacci_helper.rkt" rackunit/text-ui)
(run-tests
  (test-suite
    "fibonacci heap"
    (test-case
      "Checking makeheap creates a opaque structure"
      (define n (node 1 #f #() #f #f #f))
      (set-node-left! n n)
      (set-node-right! n n)
      (check-false (equal? (fi-heap n 1) (fi-makeheap 1))))

    (test-case
      "Checking findmin with one value"
      (check-eq? 1 (fi-findmin (fi-makeheap 1))))

    (test-case
      "checking findmin, decrement! and delete!" 
      (let ((h (fi-meld! (fi-makeheap 1) (fi-makeheap 2)))) 
       (set! h (fi-insert! h 3))
       (set! h (fi-insert! h 4))
       (set! h (fi-insert! h 5))
       (fi-deletemin! h)
       (define n (vector-ref (node-children (fi-heap-minref h)) 1)) 
       (fi-decrement! h n 2) 
       (fi-delete! h n)
       (check-eq? 2 (fi-findmin h))))

    (test-case
      "parent gets marked when one of its children are deleted"
      (let ((h (fi-meld! (fi-makeheap 1) (fi-makeheap 2))))
       (set! h (fi-insert! h 3))
       (set! h (fi-insert! h 4))
       (set! h (fi-insert! h 5))
       (fi-deletemin! h)
       (define n (fi-heap-minref h))
       (define c1 (vector-ref (fi-node-children n) 1))
       (define c2 (vector-ref (fi-node-children c1) 0))
       (fi-delete! h c2)
       (check-true (node-marked c1))))

    (test-case
      "If heap property is violated during decrement, parent's vector of children is updated correctly"
      (let ((h (fi-meld! (fi-makeheap 1) (fi-makeheap 2))))
       (set! h (fi-insert! h 3))
       (set! h (fi-insert! h 4))
       (set! h (fi-insert! h 5))
       (fi-deletemin! h)
       (define n (fi-heap-minref h))
       (define c1 (vector-ref (fi-node-children n) 1))
       (define c2 (vector-ref (fi-node-children c1) 0))
       (fi-decrement! h c2 4)
       (check-true (eq? (vector-length (fi-node-children c1)) 0))))

    (test-case 
      "if heap of size 1, used with deletemin is eventually of size 0, it should still pass through insert"
      (let ((h (fi-makeheap 1)))
       (fi-deletemin! h)
       (set! h (fi-insert! h 2))
       (check-true (eq? (fi-heap-size h) 1))
       (check-true (eq? (fi-findmin h) 2))))

    (test-case
      "If two children of a root node are deleted, it shouldn't be marked"
      (let ((h (fi-meld! (fi-makeheap 1) (fi-makeheap 2))))
       (set! h (fi-insert! h 3))
       (set! h (fi-insert! h 4))
       (set! h (fi-insert! h 5))
       (fi-deletemin! h)
       (define n (fi-heap-minref h))
       (define c1 (vector-ref (fi-node-children n) 0))
       (define c2 (vector-ref (fi-node-children n) 1))
       (fi-delete! h c1)
       (fi-delete! h c2)
       (check-false (node-marked n))
       (fi-deletemin! h)
       (check-eq? 4 (fi-findmin h))))))
