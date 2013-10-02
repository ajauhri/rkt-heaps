#lang scribble/manual

@title{Binomial heaps}
@(require (for-label "binomial.rkt"))

@defmodule[binomial]

A @deftech{Binomial heap} is a data structure for maintaining a collection of elements, such that new elements can be added and the element of minimum value extracted efficiently. This implmentation is purely functional hence @defterm{immutable}. @deftech{Binomial heap} allow only numbers to be stored in them.

@hyperlink["https://github.com/ajauhri/rkt-heaps/blob/master/src/binomial_helper.rkt" "heap-lazy?"] is just a check if the given argument complies with the binomial heap structure adopted in this implementation. @deftech{Binomial heaps} have not implemented using @racket[struct]. All values of the heap are stored in a vector which is pointed by @racket[car] of a pair. The @racket[cdr] is the count/size of the heap. This pair is embedded within another pair's @racket[car]. The @racket[cdr] of the outer pair stores the min value of the heap.

@defproc[(makeheap [val number?]) heap-lazy?]{

Returns a newly allocated heap with only one element @racket[val].}

@defproc[(findmin [h heap-lazy?]) number?]{
	
Returns a minimum value in the heap @racket[h].}

@defproc[(insert [h heap-lazy?]
		 [val number?]) heap-lazy?]{

Returns a newly allocated heap which is a copy of @racket[h] along with @racket[val].}

@defproc[(deletemin [h heap?]) heap?]{

Returns a newly allocated heap with the min value of the given heap @racket[h] removed.}

@defproc[(meld [h1 heap-lazy?]
	       [h2 heap-lazy?]) heap?]{

Returns a newly allocated heap by coupling @racket[h1] and @racket[h2].}

@defproc[(count [h heap-lazy?]) exact-nonnegative-integer?]{

Returns the count of the elements in the heap @racket[h]}
