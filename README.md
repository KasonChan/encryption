Encryption
==========

This is a repo contains the following encryption algorithms. They are written
in Scala.

*  RSA public-key encryption
*  Rabin public-key encryption (Under Construction)
*  ElGamal public-key encryption

Assumptions
-----------

I assume you have installed the following:

*  [Scala](http://www.scala-lang.org/download/)
*  [SBT](http://www.scala-sbt.org/download.html)

Running the Code
----------------

Follow these steps to run the code:

1.  `cd` into the _this_ directory.
2.  Enter `sbt run` to execute `Demo`.
3.  Enter `sbt test` to execute all the test cases in `demoTest`.
4.  Enter `sbt \"test-only\" demoTest.classname` to execute the test cases in the specified class.

References
----------

*  http://cacr.uwaterloo.ca/hac/about/chap8.pdf
*  http://en.wikipedia.org/wiki/RSA_%28cryptosystem%29
*  http://stackoverflow.com/questions/5385024/mod-in-java-produces-negative-numbers