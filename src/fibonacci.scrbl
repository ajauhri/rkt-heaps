#lang scribble/manual

@title{Fibonacci heaps}
@(require (for-label "fibonacci.rkt"))

@defmodule[fibonacci]

A @deftech{fibonacci heap} is a data structure for maintaining a collection of elements. In addition to the binomial heap operations, Fibonacci heaps provide two additional operations viz. @deftech{decrement} and @deftech{delete} exist. Although, it should be noted that that trees in @deftech{Fibonacci heaps} are not binomial trees as the implementation cuts subtrees out of them in a controlled way. The rank of a tree is the number of children of the root, and as with binomial heaps we only link two trees if they have the same rank.

@defproc[(makeheap [val number?]) heap?]{

Returns a newly allocated heap with only one element @racket[val].}

@defproc[(findmin [h heap?]) number?]{
	
Returns a minimum value in the heap @racket[h].}

@defproc[(insert! [h heap?]
		 [val number?]) void?]{

Updates the heap, @racket[h],  with a new node having  @racket[val].}

@defproc[(deletemin! [h heap?]) heap?]{

Updates the given heap @racket[h] by removing the node with the minimum value and changing the reference to the new min node.}

@defproc[(meld [h1 heap?]
	       [h2 heap?]) heap?]{

Returns a newly allocated heap by coupling @racket[h1] and @racket[h2].}

@defproc[(decrement! [h heap?]
		     [noderef node?]
		     [delta number?]) void?]{

Updates the value of @racket[noderef] and if the heap condition is violated, then parent of the @racket[noderef] is checked and if already marked, it is removed. This happens recursively until the root of the tree is reached or a parent which is not marked.}

@defproc[(delete! [h heap?]
		  [noderef node?]) void?]{

Updates the heap @racket[h] by deleting the @racket[noderef] and updating its parent and children. Here also if the parent is marked, then it is also removed from the tree and added as a root. Happens until the root of the tree is reached or a parent is not marked.}

@defproc[(heap-minref [h heap?]) node?]{

Returns the reference of the node which has the minimum value in @racket[h].}

@defproc[(heap-roots [h heap?]) vector?]{

Returns a vector with references to all roots in @racket[h]. The references are indexed in the vector based on the rank of the root.}

@defproc[(heap-size [h heap?]) exect-nonnegative-integer?]{

Returns the size of the heap @racket[h].}

@defproc[(node-val [n node?]) number?]{

Returns the value stored in the node @racket[n].}

@defproc[(node-children [n node?]) vector?]{

Returns a vector of all children of node @racket[n]. If @racket[n] does not have any children then a empty vector will be returned.}

@defproc[(node-parent [n node?]) node?]{

Returns the parent node of @racket[n] if there exists one or #f.}






