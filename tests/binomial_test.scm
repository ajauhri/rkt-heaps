(load "test-manager/load.scm")
(load "../binomial.scm")

(in-test-group
  binomial-heap
  (define-test (isheap)
    "Checking isheap? works fine"
    (assert-true (isheap? (makeheap 1)))
    (assert-false (isheap? (cons 1 0))))
  
  (define-test (findmin)
    "Checking findmin"
    (assert-= 0 (findmin (cons 3 4)))
    (assert-= 1 (findmin (makeheap 1)))))
(run-registered-tests)
