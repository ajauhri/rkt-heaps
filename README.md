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

The whitepaper is available [here](https://www.dropbox.com/s/2zmca8om1s7t8jo/final-report.pdf).

High resolution figures in the whitepaper for easy viewing are available here:

* [Binomial Heaps Anomaly(linear)](https://www.dropbox.com/s/ta9hdb4sz8t9izg/insert_binomial_linear.pdf) 
* [Binomial Heaps Anomaly(quadratic)](https://www.dropbox.com/s/zps81vwqfaxuzm9/insert_binomial_quad.pdf) 
* [Fibonacci Heaps (insert)](https://www.dropbox.com/s/tk988xsu7qmcmr3/fi_insert.pdf)
* [Fibonacci Heaps (meld)](https://www.dropbox.com/s/q30a15eqxpikge5/fi_meld.pdf) 
* [Fibonacci Heaps (delete)](https://www.dropbox.com/s/i2ut55g0k36krgd/fi_delete.pdf) 
* [Fibonacci Heaps (decrement)](https://www.dropbox.com/s/up6l1wfiaj3inh9/fi_decrement.pdf)
* [Binary vs Fibonacci (findmin)](https://www.dropbox.com/s/7rh758163bn0zbk/findmin.pdf)
* [Binary vs Fibonacci (deletemin)](https://www.dropbox.com/s/s8jl62ovd8g2t4n/deletemin.pdf)

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

