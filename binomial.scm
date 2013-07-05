;;; Book-keeper of the functions to be implemented 
;; makeheap(i) 
;; findmin(h) 
;; insert(h,i) -> meld(h,makeheap(i)) 
;; deletemin(h) 
;; meld(h,h') 

;;; Description: Return a new heap containing only element i
(define (makeheap i)
  (cons (make-vector 1 i) 0))

;;; Description: Return the min element in the heap
;; Notes: vector-ref is takes constant time - www.eecs.bekeley.edu/~bh/ssch23/vectors.html 
(define (findmin h)
  (vector-ref (car h) (cdr h)))

