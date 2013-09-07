(load "test-manager/load.scm")
(load "../binomial.scm")

(in-test-group
  binomial-heap
  
  (define-test (makeheap)
    "Checking makeheap"
    (assert-true (equal? (cons (cons (vector 1) 1) 0) (makeheap 1))))
   
  (define-test (findmin)
    "Checking findmin"
    (assert-= 1 (findmin (makeheap 1))) 
    (let ((h (insert (insert (makeheap 3) 4) 2))) (assert-= 2 (findmin h))))
  
  (define-test (insert)
    "Checking insert"
    (assert-true (heap? (insert (makeheap 1) 2))))
  
  (define-test (heaps?)
    "Checking heaps? works fine"
    (assert-true (heaps? (makeheap 1))) 
    (assert-false (heap? (cons (cons (vector #f 1 1) 2) 0)))
    (assert-true (heaps? (makeheap 1) (makeheap 2) (makeheap 3))))
 
  (define-test (meld)
    "Checking meld for not valid heaps"
    (assert-false (meld (makeheap 1) (cons (cons #(1 2 3) 3) 2)))
    (assert-false (meld (makeheap 1) (cons (cons '(1 2 3) 3) 0)))
    (assert-true (equal? (cons (cons (vector #f 1 1) 2) 1) (meld (makeheap 1) (makeheap 1))))))

(run-registered-tests)
