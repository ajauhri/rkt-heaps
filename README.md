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

For a complete description of operations provided by both heaps in this API, refer to [Binomial docs](http://htmlpreview.github.com/?https://github.com/ajauhri/rkt-heaps/blob/master/docs/binomial/index.html) or [Fibonacci docs](http://htmlpreview.github.com/?https://github.com/ajauhri/rkt-heaps/blob/master/docs/fibonacci/index.html).

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

For example, `./delete_benchmarks.rkt 10 100` will have timing of operations measured over heap sizes - 10 20 30 40 50 60 70 80 90 100 over the total expected cost (in this case it is `O(log n)`). It will generate a pdf file titled as `delete_benchmarks_10_100.pdf`.

Benchmark for existing racket library [Binary heap](http://pre.racket-lang.org/docs/html/data/Binary_Heaps.html) has been added for comparison. 

Some pre-generated benchmarks can be viewed here:

* [Binary Heaps (existing)](https://www.dropbox.com/s/55lgcn7dh6z7833/binary_100_5000_50_50.pdf) 

* [Binomial Heaps (new)](https://www.dropbox.com/s/9c1md2iwq96vx7i/binomial_100_5000_50_50.pdf) 

* [Fibonacci Heaps (new)](https://www.dropbox.com/s/jdzvi9kcgijj9u2/fibonacci_100_5000_50_50.pdf)

Running the test suite 
----

Test cases can be found [here](https://github.com/ajauhri/rkt-heaps/tree/master/tests), and individual test suite can be run using:
	
	./rkt-heaps/tests/binomial_tests
	./rkt-heaps/tests/binomial_helper_tests
	./rkt-heap/tests/fibonacci_tests

All tests can be run using:

	./rkt-heaps/tests/run_all_tests.rkt

