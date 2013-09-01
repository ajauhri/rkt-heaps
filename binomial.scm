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
;; - vector-ref is takes constant time - www.eecs.berkeley.edu/~bh/ssch23/vectors.html 
;; - if actual argument is not a heap, function returns 0
(define (findmin h)
  (if (checkheap? h)
    (vector-ref (car h) (cdr h))
    0))

;;; Description: Predicate based on the structural formation of binomial heap
(define (areheaps? h)
  (for-all? h checkheap?))

(define (checkheap? h)
  (and (vector? (car h)) (< (cdr h) (vector-length (car h)))))

;; meld function:
;; - The heap is structured in a pair with `car` pointing to a vector & `cdr` pointing to the index of the smallest root of a binomial tree in the heap. 
;; Meld operation is a bit complicated.
;; - if both heaps have b_i's, then the resultant heap will get one b_(i+1). 
;; - if more than 2, then as before two will combine to form b_(i+1) and any one will stay as b_i
(define (meld h1 h2)
  (if (areheaps? (list h1 h2))
    (if (< (findmin h1) (findmin h2))
      (cons (combine (car h1) (car h2)) 0))
    '()))

;; takes vectors only
(define (combine v1 v2 i)
  (cond ((and (not empty-slot? v1 i) (not empty-slot? v2 i))
         (let ((result (make-vector (expt 2 i) -1))) 
          (cond ((<= (value-at v1 i) (value-at v2 i))
                 (vector-append result 
                                (subvector v1 (root-index i) (root-index (+ i 1))) 
                                (subvector v2 (root-index i) (root-index (+ i 1)))
                                (combine v1 v2 (+ i 1))))
                ((> (value-at v1 i) (value-at v2 i))
                 (vector-append result 
                                (subvector v2 (root-index i) (root-index (+ i 1)))
                                (subvector v1 (root-index i) (root-index (+ i 1)))
                                (combine v1 v2 (+ i 1)))))))
        ((and (empty-slot? v1 i) (empty-slot? v2 i))
         (vector-append (make-vector (expt 2 i) -1)
                        (combine v1 v2 (+ i 1))))
        ((or (empty-slot? v1 i) (empty-slot? v2 i))
         (cond ((empty-slot? v1 i) 
                (vector-append (subvector v2 (root-index i) (root-index (+ i 1))) 
                               (combine v1 v2 (+ i 1))))
               ((empty-slot? v2 i)
                (vector-append (subvector v1 (root-index i) (root-index (+ i 1)))
                               (combine v1 v2 (+ i 1))))))))


(define (root-index i)
  (- (expt 2 i) 1))

(define (empty-slot? vec i)
  (eq? (value-at vec i) -1))

(define (value-at vec i)
  (vector-ref vec (- (expt 2 i) 1)))

(define (vector-empty? vec)
  (if (> (vector-length vec) 0)
    #t
    #f))
