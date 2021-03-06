Heaps for Racket
====

This is a functional implementation of heaps viz. Binomial and Fibonacci. For development, the following set of materials were referred:
* [Kozen](http://www.amazon.com/Analysis-Algorithms-Monographs-Computer-Science/dp/0387976876) 
* [Kevin Wayne's lecture notes](http://www.cs.princeton.edu/~wayne/teaching/fibonacci-heap.pdf)
* [Fredman & Tarjan. "Fibonacci heaps and their uses in improved network optimization algorithms"](http://www.cs.princeton.edu/courses/archive/fall03/cs528/handouts/fibonacci%20heaps.pdf)
* [Vuillemin, "A data structure for manipulating priority queues"](http://www.cl.cam.ac.uk/teaching/1011/AlgorithII/1978-Vuillemin-queues.pdf)

Brief description
----

Binomial heap is a collection of heap-ordered binomial trees with a pointer `min` to the tree whose root has minimum value. They are handy for quick merging of multiple heaps, and deleting the node with the minimal value. 

Fibonacci heaps, are a generalization of Binomial heaps allowing additional features other than those in binomial heaps. Specifically, they allow deletion of a `node` from the heap, and modification of the value of a `node` in the heap. For a more detailed description, refer to materials cited above.

The whitepaper is available [here](http://www.abhinavjauhri.com/projects/rkt-heaps/rkt-heaps.pdf).

High resolution figures in the whitepaper for easy viewing are available here:

* [Binomial Heaps Anomaly(linear)](http://abhinavjauhri.com/projects/rkt-heaps/insert_binomial_linear.pdf) 
* [Binomial Heaps Anomaly(quadratic)](http://abhinavjauhri.com/projects/rkt-heaps/insert_binomial_quad.pdf) 
* [Fibonacci Heaps (insert)](http://www.abhinavjauhri.com/projects/rkt-heaps/fi_insert.pdf)
* [Fibonacci Heaps (meld)](http://www.abhinavjauhri.com/projects/rkt-heaps/fi_meld.pdf) 
* [Fibonacci Heaps (delete)](http://www.abhinavjauhri.com/projects/rkt-heaps/fi_delete.pdf) 
* [Fibonacci Heaps (decrement)](http://www.abhinavjauhri.com/projects/rkt-heaps/fi_decrement.pdf)
* [Binary vs Fibonacci (findmin)](http://www.abhinavjauhri.com/projects/rkt-heaps/findmin.pdf)
* [Binary vs Fibonacci (deletemin)](http://www.abhinavjauhri.com/projects/rkt-heaps/deletemin.pdf)

Usage
----
Clone repository using:
	
	git clone git@github.com:ajauhri/rkt-heaps.git	

Open `racket` in terminal or add to source, using:

	(require "$RKT_HEAPS_HOME/src/binomial.rkt")
	(require "$RKT_HEAPS_HOME/src/fibonacci.rkt")

, where `$RKT_HEAPS_HOME` is the root directory for the downloaded source.

All core operations with examples are documented on the following pages:
* [Binomial docs](http://htmlpreview.github.com/?https://github.com/ajauhri/rkt-heaps/blob/master/docs/binomial/index.html) 
* [Fibonacci docs](http://htmlpreview.github.com/?https://github.com/ajauhri/rkt-heaps/blob/master/docs/fibonacci/index.html) 

Files under `benchmarks` folder also provide a good set of examples on the usage of this API.

Benchmarks
----
Benchmarks can be easily generated for all core functions using files [here](https://github.com/ajauhri/rkt-heaps/tree/master/benchmarks). Any one of such files can be run with following arguments:
* ssize - the starting size of the heap in the benchmark
* esize - the largest size of the heap to be included in the benchmark

For example, `./delete_benchmarks.rkt 10 100` will have timing of operations measured over heap sizes - 10 20 30 40 50 60 70 80 90 100 over the total expected cost (in this case it is `O(nlog n)`). It will generate a pdf file titled as `delete_benchmarks_10_100.pdf`.

Running the test suite 
----

Test cases can be found [here](https://github.com/ajauhri/rkt-heaps/tree/master/tests), and individual test suite can be run using:

	cd rkt-heaps/tests/	
	./binomial_tests
	./binomial_helper_tests
	./fibonacci_tests

All tests can be run using:

	./run_all_tests.rkt

