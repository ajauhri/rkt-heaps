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
    (let ((v1_range (- (vector-length (car h1)) 1)) (v2_range (- (vector-length (car h2)) 1)))
     ;; Description: Takes vectors only as input arguments and returns the resultant heap/vector. The combination is done with the following rules:
     ;; - if both heaps have h_i's, then the resultant heap will get one b_(i+1). 
     ;; - if more than 2 h_i's, then as before two will combine to form b_(i+1) and any one will stay as h_i
     ;; - if neither heaps have a h_i, then the resultant shall also not have one unless not carried forward from h_(i-1)
     ;; - if either one of the heaps have h_i, then it is also the h_i for the combined heap
     (define (combine v1 v2 carry i)
       (cond ((and (< v1_range (root-index i)) (< v2_range (root-index i))) carry)
             ((< v1_range (root-index i)) (vector-append (compute-subvector v2 carry i) (combine v1 v2 (compute-carry v2 carry i) (+ i 1))))
             ((< v2_range (root-index i)) (vector-append (compute-subvector v1 carry i) (combine v1 v2 (compute-carry v1 carry i) (+ i 1))))
             ((empty-slot? v1 i) (vector-append (compute-subvector v2 carry i) (combine v1 v2 (compute-carry v2 carry i) (+ i 1))))
             ((empty-slot? v2 i) (vector-append (compute-subvector v1 carry i) (combine v1 v2 (compute-carry v1 carry i) (+ i 1))))
             
             ((and (not (empty-slot? v1 i)) (not (empty-slot? v2 i)))
                 (cond ((<= (value-at v1 i) (value-at v2 i))
                      (vector-append (if (> (vector-length carry) 0) carry (make-vector (expt 2 i) -1))
                                     (combine v1 v2 
                                              (vector-append (subvector v1 (root-index i) (root-index (+ i 1)))
                                                             (subvector v2 (root-index i) (root-index (+ i 1))))
                                              (+ i 1))))
                     ((> (value-at v1 i) (value-at v2 i))
                      (vector-append (if (> (vector-length carry) 0) carry (make-vector (expt 2 i) -1)) 
                                     (combine v1 v2 
                                              (vector-append (subvector v2 (root-index i) (root-index (+ i 1)))
                                                             (subvector v1 (root-index i) (root-index (+ i 1))))
                                              (+ i 1))))))))
     (cons (combine (car h1) (car h2) #()  0) 0)) 
    '()))

(define (insert h i)
  (meld h (makeheap i)))

;;; HELPER FUNCTIONS ;;;
(define (compute-carry v carry i)
  (if (and (= (vector-length carry) (expt 2 i)) (not (empty-slot? v i)))
    (if (<= (value-at v 1) (vector-ref carry 0))
      (vector-append (subvector v (root-index i) (root-index (+ i 1))) carry)
      (vector-append carry (subvector v (root-index i) (root-index (+ i 1)))))
    #()))

(define (compute-subvector v carry i)
  (let ((len (expt 2 i)) 
        (root-empty? (empty-slot? v i)))
   (cond ((and (not (= (vector-length carry) len)) (not root-empty?)) 
         (subvector v (root-index i) (root-index (+ i 1))))
        ((and (not (= (vector-length carry) len)) root-empty?) 
         (make-vector (expt 2 i) -1))
        ((and (= (vector-length carry) len) (not root-empty?))
         (make-vector (expt 2 i) -1))
        ((and (= (vector-length carry) len) root-empty?)
         carry))))
 

;;; Description: Returns the vector index of the root of a tree in the heap
(define (root-index i)
  (- (expt 2 i) 1))

;;; Description: Returns whether the root of a tree in the heap is vacant or not
(define (empty-slot? vec i)
  (eq? (value-at vec i) -1))

;;; Description: Returns the root element of a tree in the heap
(define (value-at vec i)
  (vector-ref vec (root-index i)))
