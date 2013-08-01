(load "test-manager/load.scm")
(load "../binomial.scm")

(in-test-group
  binomial-heap
  (define-test (isheap)
    "Checking isheap? works fine"
    (define h (makeheap 1))
    (assert-true (isheap? h))
    (assert-false (isheap? (cons 1 0)))))

(run-registered-tests)
