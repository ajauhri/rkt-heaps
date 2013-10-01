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
Clone repository using:
	
	git clone git@github.com:ajauhri/rkt-heaps.git	

Open `racket` in terminal, add interfaces using:

	(require "$RKT_HEAPS_HOME/src/binomial.rkt")
	(require "$RKT_HEAPS_HOME/src/fibonacci.rkt")

, where `$RKT_HEAPS_HOME` is the root directory for the downloaded source

All functions exposed are provided in the header section of each heap file viz. [binomial.rkt](https://github.com/ajauhri/rkt-heaps/blob/master/src/binomial.rkt) and [fibonacci.rkt](https://github.com/ajauhri/rkt-heaps/blob/master/src/fibonacci.rkt) 

Benchmarks
----
Benchmarks can be easily generated for all core functions using files [here](https://github.com/ajauhri/rkt-heaps/tree/master/benchmarks). Any one of such files can be run with following arguments:
* ssize - the starting size of the heap in the benchmark
* esize - the largest size of the heap to be included in the benchmark
* step - increment in the size of heap for the subsequent step 
* freq - # of execution cycles to be run for any method 

For example, `./binomial_benchmarks.rkt 10 100 10 10` will have timing of methods measured over heap sizes - 10 20 30 40 50 60 70 80 90 100, for all methods 10 times, and then take its mean. It will generate a pdf file titled as `binomial_benchmarks_10_100_10_10.pdf`.

Benchmark for existing racket library [Binary heap](http://pre.racket-lang.org/docs/html/data/Binary_Heaps.html) has been added for comparison. `Binomial` & `Fibonacci` heap implementations provide a great amount of speedup for `heap-add-all!`, belonging to `Binary heap`, operation which is similar to `meld` operation for `Binomial` and `Fibonacci` heap in this implementation.

You can also see some pre-generated benchmarks here:

* [Binary Heaps (existing)](https://www.dropbox.com/s/55lgcn7dh6z7833/binary_100_5000_50_50.pdf) 

* [Binomial Heaps (new)](https://www.dropbox.com/s/9c1md2iwq96vx7i/binomial_100_5000_50_50.pdf) 

* [Fibonacci Heaps (new)](https://www.dropbox.com/s/jdzvi9kcgijj9u2/fibonacci_100_5000_50_50.pdf)

Running the test suite 
----

Test cases can be found [here](https://github.com/ajauhri/rkt-heaps/tree/master/tests), and individuals test suite can be run using:
	
	./rkt-heaps/tests/binomial_tests
	./rkt-heaps/tests/binomial_helper_tests
	./rkt-heap/tests/fibonacci_tests

All tests can be run using:

	./rkt-heaps/tests/run_all_tests.rkt





