#! /usr/bin/env racket
#lang racket

(require rackunit "../src/binomial_helper.rkt" "../src/binomial.rkt" rackunit/text-ui)

(run-tests
  (test-suite
    "binomial helper functions binomail heap"

    (test-case
      "Checking insert"
      (check-true (heap? (bino-insert (bino-makeheap 1) 2))))

    (test-case
      "Checking heap-lazy? and heap? works fine"
      (check-true (heap-lazy? (cons (cons (vector 1 1) 2) #f))) 
      (check-false (heap? (cons (cons (vector #f 1 1) 2) 0))))))

