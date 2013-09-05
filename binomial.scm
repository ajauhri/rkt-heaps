;;; Book-keeper of the functions to be implemented 
;; makeheap(i) 
;; isheap? 
;; findmin(h) 
;; insert(h,i) -> meld(h,makeheap(i)) 
;; deletemin(h) 
;; meld(h,h') 

;;; Description: Returns a new heap containing only element
;; Commentary:
;; - the heap is structured in a pair with `car` pointing to a vector and `cdr` pointing to the index of the smallest root of a binomial tree in the heap.
(define (makeheap i)
  (cons (make-vector 1 i) 0))

;;; Description: Returns the min element in the heap
;; Commentary: 
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

;;; Description: Returns a heap which a combination of the two heaps provided as arguments to the method. 
;; todo: find the resultant min. element of the combined heap
(define (meld h1 h2)
  (if (areheaps? (list h1 h2))
    (let ()
      (define (couple v1 v2 carry res i)
              (let ((b1 (root-slot-valid? v1 i)) 
                    (b2 (root-slot-valid? v2 i)) 
                    (b3 (if (> (vector-length carry) 0) #t #f)))
                (cond ((and (not b1) (not b2) (not b3)) res)
                      (else
                        (let ((newargs 
                                (cond ((and (not b1) (not b2) b3) (cons #() (vector-append res carry))) 
                                      ((and (not b1) b2 (not b3)) (cons #() (vector-append res (subvector v2 (root-index i) (root-index (+ i 1))))))
                                      ((and (not b1) b2 b3) (cons (propcarry v2 carry i) (vector-append res (make-vector (expt 2 i) #f))))
                                      ((and b1 (not b2) (not b3)) (cons #() (vector-append res (subvector v1 (root-index i) (root-index (+ i 1))))))
                                      ((and b1 (not b2) b3) (cons (propcarry v1 carry i) (vector-append res (make-vector (expt 2 i) #f))))
                                      ((and b1 b2 (not b3)) (cons (constructcarry v1 v2 i) (vector-append res (make-vector (expt 2 i) #f))))
                                      ((and b1 b2 b3) (cons (propcarry v2 carry i) (vector-append res (subvector v1 (root-index i) (root-index (+ i 1)))))))))
                          (couple v1 v2 (car newargs) (cdr newargs) (+ i 1)))))))
      (cons (couple (car h1) (car h2) #() #() 0) 0))
    '()))

;; Description: Takes vectors only as input arguments and returns the resultant heap/vector. The combination is done with the following rules:
;; - if both heaps have h_i's, then the resultant heap will get one h_(i+1). 
;; - if more than 2 h_i's, then as before two will combine to form h_(i+1) and any one will stay as h_i
;; - if neither heaps have a h_i, then the resultant shall also not have one unless not carried forward from h_(i-1)
;; - if either one of the heaps have h_i, then it is also the h_i for the combined heap 


(define (propcarry v c i) 
  (if (<= (vector-ref c 0) (vector-ref v (root-index i))) 
    (vector-append c (subvector v (root-index i) (root-index (+ i 1))))
    (vector-append (subvector v (root-index i) (root-index (+ i 1))) c)))

(define (constructcarry v1 v2 i)
  (let ((start (root-index i)) (end (root-index (+ i 1))))
   (if (<= (vector-ref v1 (root-index i)) (vector-ref v2 (root-index i)))
     (vector-append (subvector v1 start end) (subvector v2 start end))
     (vector-append (subvector v2 start end) (subvector v1 start end)))))

;;; Description: Insert a positive integer to an existing heap and returns the resultant heap
(define (insert h i)
  (meld h (makeheap i)))

;;; Description: Returns the vector index of the root of a tree in the heap
(define (root-index i)
  (- (expt 2 i) 1))

;;; Description: Returns whether the root of a tree in the heap is vacant or not
(define (root-slot-valid? vec i)
  (and (> (vector-length vec) (root-index i)) (specified? vec (root-index i))))

(define (specified? v i)
  (if (not (vector-ref v i)) #f #t))
