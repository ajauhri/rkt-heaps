Heaps for Racket
====

This is a functional implementation of heaps viz. Binomial and Fibonacci. For the development I referred following materials:
* [Kozen](http://www.amazon.com/Analysis-Algorithms-Monographs-Computer-Science/dp/0387976876) 
* [Kevin Wayne's lecture notes](http://www.cs.princeton.edu/~wayne/teaching/fibonacci-heap.pdf)
* [Fredman & Tarjan. "Fibonacci heaps and their uses in improved network optimization algorithms"](http://www.cs.princeton.edu/courses/archive/fall03/cs528/handouts/fibonacci%20heaps.pdf)
* [Vuillemin, "A data structure for manipulating priority queues"](http://www.cl.cam.ac.uk/teaching/1011/AlgorithII/1978-Vuillemin-queues.pdf)

The implementation for Binomial heaps is purely functional whereas for Fibonacci heaps there exists destructive (mutating) methods.

Binomial heap is a collection of binomial trees which may be handy for quick merging of heaps; delete min to name some features. Fibonacci heaps, are a generalization of Binomial heaps allowing additional features other than those in binomial heaps. Particularly, they allow to deletion of a particular node, and modification of its value. 

Usage
----


Benchmarks
----


Running the test suite 
----






