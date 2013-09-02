(load "test-manager/load.scm")
(load "../binomial.scm")

(in-test-group
  binomial-heap
  (define-test (isheap)
    "Checking isheap? works fine"
    (assert-true (areheaps? (list (makeheap 1))))
    (assert-false (areheaps? (list (cons 1 0)))))
  
  (define-test (findmin)
    "Checking findmin"
    (assert-= 0 (findmin (cons 3 4)))
    (assert-= 1 (findmin (makeheap 1))))
  
  (define-test (meld)
    "Checking meld for not valid heaps"
    (assert-true (null? (meld (makeheap 1) (cons 1 2))))
    (assert-equal (cons (vector '1 1) 0) (meld (makeheap 1) (makeheap 1))))
  
  (define-test (root-index)
               "Checking root index gives the correct index for root value in the vector"
               (assert-equal 0 (rootindex 0)))
  
  (define-test (empty-slot)
               "Checking empty slot in the vector"
               (assert-true (emptyslot? (make-vector 1 -1) 0))
               (assert-false (emptyslot? (vector '1 '2 '3) 1))
               (assert-true (emptyslot? (vector '1 '2 '3 '-1) 2)))
  
  (define-test (value-at)
               "Checking for correct value extraction from the vector"
               (assert-equal 2 (valueat (vector '1 '2 '3) 1))))

(run-registered-tests)
