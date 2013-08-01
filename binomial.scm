;;; Book-keeper of the functions to be implemented 
;; makeheap(i) 
;; isheap? 
;; findmin(h) 
;; insert(h,i) -> meld(h,makeheap(i)) 
;; deletemin(h) 
;; meld(h,h') 

;;; Description: Return a new heap containing only element i
(define (makeheap i)
  (cons (make-vector 1 i) 0))

;;; Description: Return the min element in the heap
;; Notes: 
;; - vector-ref is takes constant time - www.eecs.bekeley.edu/~bh/ssch23/vectors.html 
;; - if actual argument is not a heap, function returns 0
(define (findmin h)
  (if (isheap? h)
    (vector-ref (car h) (cdr h))
    0))

;;;(define insert 
;;;  (compose meld makeheap))

;;; Description: Predicate based on the structural formation of binomial heap
(define (isheap? h)
  (and (vector? (car h)) (< (cdr h) (vector-length (car h)))))

;; meld function:
;; - The heap is structured in a pair with `car` pointing to a vector & `cdr` pointing to the index of the smallest root of a binomial tree in the heap. In cases where the heap consists of just one tree the root has to have the smallest element in which the having this pair structure may seem irrelevant but let's just work on it for now. Meld operation is a bit complicated so I want to list out the minor details in my mind before implementation.
;; - if both heaps have b_i's, then the resultant heap will get one b_(i+1). 
;; - if more than 2, then as before two will combine to form b_(i+1) and any one will stay as b_i
;(define (meld h1 h2))
