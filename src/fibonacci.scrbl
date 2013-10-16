#lang scribble/manual

@(require scribble/eval (for-label "fibonacci.rkt"))

@title{Fibonacci heaps}

@defmodule[fibonacci #:use-sources ("fibonacci.rkt")]

Fibonacci Heaps

@(define the-eval (make-base-eval))
@(the-eval '(require "fibonacci.rkt"))

A @deftech{Fibonacci heap} is a data structure for maintaining a collection of elements. In addition to the binomial heap operations, Fibonacci heaps provide two additional operations viz. @deftech{decrement} and @deftech{delete} exist. Although, it should be noted that that trees in @deftech{Fibonacci heaps} are not binomial trees as the implementation cuts subtrees out of them in a controlled way. The rank of a tree is the number of children of the root, and as with binomial heaps we only link two trees if they have the same rank. 

Roots of binomial tress in the heap are stored in the form of a doubly linked list; each node has a reference to a left and right node.

@defproc[(fi-makeheap [val number?]) fi-heap?]{

Returns a newly allocated heap with only one element @racket[val].
@examples[#:eval the-eval
	(define h (fi-makeheap 3))
	h]}

@defproc[(fi-findmin [h fi-heap?]) number?]{
	
Returns a minimum value in the heap @racket[h].
@examples[#:eval the-eval
	(define h (fi-meld! (fi-makeheap 1) (fi-makeheap 2)))
	(fi-findmin h)]}

@defproc[(fi-insert! [h fi-heap?]
		 [val number?]) void?]{

Updates the left right pointers of the min node to accommodate the new node with @racket[val], @racket[h],  with a new node having  @racket[val].
@examples[#:eval the-eval
	(define h (fi-makeheap 1))
	(set! h (fi-insert! h 2))
	(fi-heap-size h)]}

@defproc[(fi-deletemin! [h fi-heap?]) fi-heap?]{

Updates the given heap @racket[h] by removing the node with the minimum value and changing the reference to the new min node.
@examples[#:eval the-eval
	(define h (fi-meld! (fi-makeheap 1) (fi-makeheap 2)))
	(fi-deletemin! h)
	(fi-findmin h)]}

@defproc[(fi-meld! [h1 fi-heap?]
	       [h2 fi-heap?]) fi-heap?]{

Returns a newly allocated heap by coupling @racket[h1] and @racket[h2].
@examples[#:eval the-eval
	(define h (fi-meld! (fi-makeheap 1) (fi-makeheap 2)))
	h]}

@defproc[(fi-decrement! [h fi-heap?]
		     [noderef node?]
		     [delta number?]) void?]{

Updates the value of @racket[noderef] and if the heap condition is violated, then parent of the @racket[noderef] is checked and if already marked, it is removed. This happens recursively until the root of the tree is reached or a parent which is not marked.
@examples[#:eval the-eval
	(define h (fi-meld! (fi-makeheap 1) (fi-makeheap 2)))
	(fi-decrement! h (fi-heap-minref h) 2)
	(fi-findmin h)]}

@defproc[(fi-delete! [h fi-heap?]
		  [noderef node?]) void?]{

Updates the heap @racket[h] by deleting the @racket[noderef] and updating its parent and children. Here also if the parent is marked, then it is also removed from the tree and added as a root. Happens until the root of the tree is reached or a parent is not marked.
@examples[#:eval the-eval
	(define h (fi-meld! (fi-makeheap 1) (fi-makeheap 2)))
	(fi-delete! h (fi-heap-minref h))
	(fi-findmin h)
	(fi-heap-size h)]}

@defproc[(fi-heap-minref [h fi-heap?]) node?]{

Returns the reference of the node which has the minimum value in @racket[h].}

@defproc[(fi-heap-size [h fi-heap?]) exect-nonnegative-integer?]{

Returns the size of the heap @racket[h].}

@defproc[(fi-node-val [n node?]) number?]{

Returns the value stored in the node @racket[n].}

@defproc[(fi-node-children [n node?]) vector?]{

Returns a vector of all children of node @racket[n]. If @racket[n] does not have any children then a empty vector will be returned.}

@defproc[(fi-node-parent [n node?]) node?]{

Returns the parent node of @racket[n] if there exists one or #f.}

@defproc[(fi-node-left [n node?]) node?]{

Returns @racket[n]'s left node in the circular linked list. If @racket[n] is the only node in list then a reference of @racket[n] will be returned.}

@defproc[(fi-node-right [n node?]) node?]{

Returns @racket[n]'s right node in the circular linked list.}












