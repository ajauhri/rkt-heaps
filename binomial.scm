;; Book-keeper of the functions to be implemented 
;; makeheap(i) 
;; findmin(h) 
;; insert(h,i)  
;; deletemin(h) 
;; meld(h,h') 

;; Returns a new heap containing only element
;; Commentary:
;; - the heap is structured as a vector along with number of values as a pair with `car` pointing to the vector and `cdr` to the number of values in the heap. To make min a constant time operation, the index to the min (root of a tree in the forest) is also stored as the `cdr` of another pair.
(define (makeheap i)
  (cons (cons (make-vector 1 i) 1) 0))

;; Returns the min element in the heap
;; Commentary: 
;;- vector-ref is takes constant time - www.eecs.berkeley.edu/~bh/ssch23/vectors.html 
;;- if actual argument is not a heap, function returns 0
(define (findmin h)
  (if (checkheap? h)
    (vector-ref (car (car h)) (cdr h))
    #f))


;;Returns a heap which a combination of the two heaps provided as arguments to the method. 
;; todo: find the resultant min. element of the combined heap
(define (meld h1 h2)
  (if (areheaps? (list h1 h2))
    (let ()
      (define (couple v1 v2 s1 s2 carry res i)
              (let ((b1 (root-slot-valid? s1)) 
                    (b2 (root-slot-valid? s2)) 
                    (b3 (if (> (vector-length carry) 0) #t #f)))
                (cond ((and (= s1 0) (= s2 0) (not b3)) res)
                      (else
                        (let ((newargs 
                                (cond ((and (not b1) (not b2) (not b3)) (cons #() (vector-append res (make-vector i #f))))
                                      ((and (not b1) (not b2) b3) (cons #() (vector-append res carry))) 
                                      ((and (not b1) b2 (not b3)) (cons #() (vector-append res (propres v2 i))))
                                      ((and (not b1) b2 b3) (cons (propcarry v2 carry i) (vector-append res (make-vector i #f))))
                                      ((and b1 (not b2) (not b3)) (cons #() (vector-append res (propres v1 i))))
                                      ((and b1 (not b2) b3) (cons (propcarry v1 carry i) (vector-append res (make-vector i #f))))
                                      ((and b1 b2 (not b3)) (cons (constructcarry v1 v2 i) (vector-append res (make-vector i #f))))
                                      ((and b1 b2 b3) (cons (propcarry v2 carry i) (vector-append res (propres v1 i)))))))
                          (couple v1 v2 (floor (/ s1 2)) (floor (/ s2 2)) (car newargs) (cdr newargs) (* i 2)))))))
      (let ((res (cons (couple (car (car h1)) (car (car h2)) (cdr (car h1)) (cdr (car h2)) #() #() 1) (+ (cdr (car h1)) (cdr (car h2))))))
       (define (getmin h len i m)
         (if (= len 0) 
           (- m 1) 
           (getmin h (floor (/ len 2)) (* i 2) (cond ((and (= (modulo len 2) 1) (eq? m #f)) i)
                                                     ((= (modulo len 2) 0) m)
                                                     (else
                                                       (if (< (vector-ref h (- i 1)) (vector-ref h (- m 1))) i m))))))
       (cons res (getmin (car res) (cdr res) 1 #f))))
    #f))


;;Insert a positive integer to an existing heap and returns the resultant heap
(define (insert h i)
  (meld h (makeheap i)))

;;;
;;; Helper functions
;;;

;; Predicate based on the structural formation of binomial heap.
(define (areheaps? h)
  (if (list? h)
    (for-all? h checkheap?)
    #f))

;; Asserts based on the underlying implementation here, whether the given parameter is structurally correct as a heap or not. Does not check whether the heap condition is violated or not.
(define (checkheap? h)
  (and (vector? (car (car h))) (< (cdr h) (vector-length (car (car h))))))

;; Returns ith bit of v
(define (propres v i)
  (subvector v (- i 1) (- (* i 2) 1)))

;; Adds ith bit of v to the carry for propagation. Maintains the heap condition
(define (propcarry v c i) 
  (if (<= (vector-ref c 0) (vector-ref v (- i 1))) 
    (vector-append c (subvector v (- i 1) (- (* i 2) 1)))
    (vector-append (subvector v (- i 1) (- (* i 2) 1)) c)))

;; Constructs ith bit of v1 and v2 as carry for propagation. The construction maintains the heap condition.
(define (constructcarry v1 v2 i)
  (let ((start (- i 1)) (end (- (* i 2) 1)))
   (if (<= (vector-ref v1 start) (vector-ref v2 start))
     (vector-append (subvector v1 start end) (subvector v2 start end))
     (vector-append (subvector v2 start end) (subvector v1 start end)))))

;; Returns whether the root of a tree in the heap is vacant or not
(define (root-slot-valid? s)
  (cond ((= (modulo s 2) 1) #t)
        ((= (modulo s 2) 0) #f))) 
