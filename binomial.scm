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
  (if (heap? h)
    (vector-ref (car (car h)) (cdr h))
    #f))


;;Insert a positive integer to an existing heap and returns the resultant heap
(define (insert h i)
  (meld h (makeheap i)))

(define (deletemin h)
  (if (heap? h)
    #t
    #f))

;;Returns a heap which a combination of the two heaps provided as arguments to the method. 
(define (meld h1 h2)
  (if (heaps? h1 h2)
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
      (let ((res (cons (couple (car (car h1)) (car (car h2)) (cdr (car h1)) (cdr (car h2)) #() #() 1) 
                       (+ (cdr (car h1)) (cdr (car h2))))))
       (cons res 
             (getmin (car res) (cdr res) 1 #f))))
    #f))

;;;
;;; Helper functions
;;;

(define (getmin v len i m) 
  (if (= len 0)
    (- m 1)
    (getmin v (floor (/ len 2)) (* i 2) (cond ((and (= (modulo len 2) 1) (eq? m #f)) i)
                                              ((= (modulo len 2) 0) m)
                                              (else
                                                (if (< (vector-ref v (- i 1)) (vector-ref v (- m 1))) i m))))))

;; Predicate based on the structural formation of binomial heap.
(define (heaps? . h)
  (for-all? h heap?))

;; Asserts whether the given argument is a valid heap or not
;; Commentary:
;; - based on the underlying implementation structural formation of a heap, it checks whether they are correctly specified in the argument
;; - it is checked whether the values of the heap should be stored in a vector
;; - it is checked that the index of the min element is at most equal to vector-lenght/2 (since the vector length will have padded #f in all trees whose root index is represented by 0 bit when writing the number of elements in binary representation 
;; - it is checked that the number of valid elements in the heap are at most the length of the entire forest(vector) 
;; - it is checked that the min is correctly indexed 
;; - doesn't check whether the heap condition is maintained or not
;; - although this is not a thorough check of the validity of the heap condition, its use is relevant since it validates what is passed to meld and deletemin methods 
(define (heap? h)
  (and (vector? (car (car h))) 
       (<= (cdr h) (floor (/ (vector-length (car (car h))) 2))) 
       (<= (cdr (car h)) (vector-length (car (car h))))
       (= (getmin (car (car h)) (cdr (car h)) 1 #f) (cdr h))))

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
