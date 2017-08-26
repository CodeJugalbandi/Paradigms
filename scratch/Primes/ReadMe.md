Finding First n Primes
====

**BRAHMA** Let us take a very simple problem of finding primes until a given number.  I'll do this in Haskell.

```haskell
isPrime :: Int -> Bool
isPrime n = filter (\x -> n `mod` x == 0) [2..n] == [2,n] 

primes :: Int -> [Int]
primes n = filter isPrime [1..n]

main :: IO ()
main = do
  print $ primes 13
  
-- Answer: [2,3,5,7,11,13]
```

**KRISHNA** Let me show you how this can be done in APL. As we know, ```⍳``` generates a vector of numbers.  In our case we use ```n``` as ```13```:

```apl
    n ← 13
    ⍳n
⍝ 1 2 3 4 5 6 7 8 9 10 11 12 13
```

**KRISHNA** Further, I can now use outer-residue and generate a table like this:

```apl
    n ← 13
    (⍳n)∘.|⍳n

⍝ 0 0 0 0 0 0 0 0 0  0  0  0 0
⍝ 1 0 1 0 1 0 1 0 1  0  1  0 1
⍝ 1 2 0 1 2 0 1 2 0  1  2  0 1
⍝ 1 2 3 0 1 2 3 0 1  2  3  0 1
⍝ 1 2 3 4 0 1 2 3 4  0  1  2 3
⍝ 1 2 3 4 5 0 1 2 3  4  5  0 1
⍝ 1 2 3 4 5 6 0 1 2  3  4  5 6
⍝ 1 2 3 4 5 6 7 0 1  2  3  4 5
⍝ 1 2 3 4 5 6 7 8 0  1  2  3 4
⍝ 1 2 3 4 5 6 7 8 9  0  1  2 3
⍝ 1 2 3 4 5 6 7 8 9 10  0  1 2
⍝ 1 2 3 4 5 6 7 8 9 10 11  0 1
⍝ 1 2 3 4 5 6 7 8 9 10 11 12 0
```
**KRISHNA** The places where ```0```s are present means we have factors at those positions. With this, all I need to do is equate it to ```0```.  This will produce ```1``` at positions where we have factors:


```apl
    n ← 13
    0=(⍳n)∘.|⍳n

⍝ 1 1 1 1 1 1 1 1 1 1 1 1 1
⍝ 0 1 0 1 0 1 0 1 0 1 0 1 0
⍝ 0 0 1 0 0 1 0 0 1 0 0 1 0
⍝ 0 0 0 1 0 0 0 1 0 0 0 1 0
⍝ 0 0 0 0 1 0 0 0 0 1 0 0 0
⍝ 0 0 0 0 0 1 0 0 0 0 0 1 0
⍝ 0 0 0 0 0 0 1 0 0 0 0 0 0
⍝ 0 0 0 0 0 0 0 1 0 0 0 0 0
⍝ 0 0 0 0 0 0 0 0 1 0 0 0 0
⍝ 0 0 0 0 0 0 0 0 0 1 0 0 0
⍝ 0 0 0 0 0 0 0 0 0 0 1 0 0
⍝ 0 0 0 0 0 0 0 0 0 0 0 1 0
⍝ 0 0 0 0 0 0 0 0 0 0 0 0 1
```
**KRISHNA** Now, I just need to do a column sum using the Slash-Bar operator ```⌿``` and plus function ```+``` like this:

```apl
  +⌿0=(⍳n)∘.|⍳n
⍝ 1 2 2 3 2 4 2 4 3 4 2 6 2
```

**KRISHNA** If you observe the output carefully, we have ```2```s where the number has a factor of ```1``` and itself, which means we have primes present at those positions.  So, equating that to ```2``` gives us positions of primes:

```apl
    2=+⌿0=(⍳n)∘.|⍳n
⍝ 0 1 1 0 1 0 1 0 0 0 1 0 1
```
**KRISHNA** We are almost there, if I just get the indices where ```1```s are present.  I do that using the iota-underbar ```⍸``` function:

```apl
    ⍸2=+⌿0=(⍳n)∘.|⍳n
⍝ 2 3 5 7 11 13
```

**BRAHMA**  Interesting, let me do that in Scala this time.  I'll use for-comprehensions, equivalent to List comprehensions in Haskell, Erlang or Python.  This generates a big vector, marking with ```1```s all the positions where factors are present and ```0```s to indicate no factors:

```scala
val until = 13

val vector = for {
  i <- 1 to until
  j <- 1 to until
} yield if (j % i == 0) 1 else 0

// Vector(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1)

```

**BRAHMA**  Further, this is not grouped like a table.  So, I'll reshape it to a matrix or table.

```scala
val until = 13

val vector = for {
  i <- 1 to until
  j <- 1 to until
} yield if (j % i == 0) 1 else 0

val matrix = vector.grouped(until).toVector

// Vector(Vector(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1), 
//        Vector(0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0), 
//        Vector(0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0), 
//        Vector(0, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0, 1, 0), 
//        Vector(0, 0, 0, 0, 1, 0, 0, 0, 0, 1, 0, 0, 0), 
//        Vector(0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 1, 0), 
//        Vector(0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0), 
//        Vector(0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0), 
//        Vector(0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0), 
//        Vector(0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0), 
//        Vector(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0), 
//        Vector(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0), 
//        Vector(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1))
```

**BRAHMA**  Having done that, I need to do a column-sum.  But I don't have a column-wise reduce operator Slash-Bar that you have in APL.  So, I'll resort to transposing this matrix and then summing the rows like this:

```scala
val until = 13

val vector = for {
  i <- 1 to until
  j <- 1 to until
} yield if (j % i == 0) 1 else 0

val matrix = vector.grouped(until).toVector.transpose
val summed = matrix.map(_.sum)

// Vector(1, 2, 2, 3, 2, 4, 2, 4, 3, 4, 2, 6, 2)
```
**BRAHMA** Now I have the ```2```s at positions where number has a factor of ```1``` and itself.  All I need is indices at that position, so by using ```zipWithIndex``` is how I achieve that.  In Scala, index starts at 0, so I've to add ```1```:

```scala
val until = 13

val vector = for {
  i <- 1 to until
  j <- 1 to until
} yield if (j % i == 0) 1 else 0

val matrix = vector.grouped(until).toVector.transpose
val summed = matrix.map(_.sum)

summed.zipWithIndex.collect { case (value, idx) if (value == 2) => idx + 1}
// Vector(2, 3, 5, 7, 11, 13)
```

**BRAHMA** Now that we have the same output, I'll simply shove all of the above into a function ```primes```:

```scala
def primes(until: Int): Vector[Int] = {
  val vector = for {
    i <- 1 to until
    j <- 1 to until
  } yield if (j % i == 0) 1 else 0

  val matrix = vector.grouped(until).toVector.transpose
  val summed = matrix.map(_.sum)

  summed.zipWithIndex.collect { case (value, idx) if (value == 2) => idx + 1}
}

println(primes(13)) // Vector(2, 3, 5, 7, 11, 13)
```
**KRISHNA**  Can you time this and see how this code performs for higher values of ```until```?

**BRAHMA**  Sure!  Let me introduce a rudimentary timing aspect here and do the measurements:

```scala
def primes(until: Int): Vector[Int] = {
  val vector = for {
    i <- 1 to until
    j <- 1 to until
  } yield if (j % i == 0) 1 else 0

  val matrix = vector.grouped(until).toVector.transpose
  val summed = matrix.map(_.sum)
  summed.zipWithIndex.collect { case (value, idx) if (value == 2) => idx + 1}
}

def time[T, R](f: Function[T, R]): Function[T, R] = {
  return t => {
    val startTime = System.currentTimeMillis
    val result = f(t)
    val diff = System.currentTimeMillis - startTime
    println(s"Time Taken = $diff(ms).")
    result
  }
}

// println(time(primes)(13)) //37ms
// println(time(primes)(130)) //160ms
// println(time(primes)(1300)) //554ms
println(time(primes)(13000)) //28442ms
println("Done")
```
**BRAHMA** Another important thing to note is - by employing rudimentary measurement we have used a very broad brush.  Compilation of JVM application is not same as that of a statically compiled language.   Look at this documentation [Parallel Collections - Performance](http://docs.scala-lang.org/overviews/parallel-collections/performance.html)


> The Java and Scala compilers convert source code into JVM bytecode and do very little optimization. On most modern JVMs, once the program bytecode is run, it is converted into machine code for the computer architecture on which it is being run. This is called the just-in-time compilation. The level of code optimization is, however, low with just-in-time compilation, since it has to be fast. To avoid recompiling, the so called HotSpot compiler only optimizes parts of the code which are executed frequently. What this means for the benchmark writer is that a program might have different performance each time it is run. Executing the same piece of code (e.g. a method) multiple times in the same JVM instance might give very different performance results depending on whether the particular code was optimized in between the runs. Additionally, measuring the execution time of some piece of code may include the time during which the JIT compiler itself was performing the optimization, thus giving inconsistent results.
> 
> Another hidden execution that takes part on the JVM is the automatic memory management. Every once in a while, the execution of the program is stopped and a garbage collector is run. If the program being benchmarked allocates any heap memory at all (and most JVM programs do), the garbage collector will have to run, thus possibly distorting the measurement. To amortize the garbage collection effects, the measured program should run many times to trigger many garbage collections.
> 
> One common cause of a performance deterioration is also boxing and unboxing that happens implicitly when passing a primitive type as an argument to a generic method. At runtime, primitive types are converted to objects which represent them, so that they could be passed to a method with a generic type parameter. This induces extra allocations and is slower, also producing additional garbage on the heap.

**KRISHNA**  Ok, so let me show you how this benchmarking can be done in APL.  All I've to do is add ```] Runtime``` in front of the expression:

```apl
n ← 13
]RUNTIME ⍸2=+⌿0=(⍳n)∘.|⍳n

* Benchmarking "⍸2=+⌿0=(⍳n)∘.|⍳n"
             (ms) 
 CPU (avg):     0 
 Elapsed:       0 

 
n ← 130
]RUNTIME ⍸2=+⌿0=(⍳n)∘.|⍳n
* Benchmarking "⍸2=+⌿0=(⍳n)∘.|⍳n"
             (ms) 
 CPU (avg):     0 
 Elapsed:       0 


n ← 1300
]RUNTIME ⍸2=+⌿0=(⍳n)∘.|⍳n
* Benchmarking "⍸2=+⌿0=(⍳n)∘.|⍳n"
             (ms) 
 CPU (avg):     6 
 Elapsed:       6 


n ← 13000
]RUNTIME ⍸2=+⌿0=(⍳n)∘.|⍳n
* Benchmarking "⍸2=+⌿0=(⍳n)∘.|⍳n"
             (ms) 
 CPU (avg):   743 
 Elapsed:     743 

```

**BRAHMA** These are interesting results.  APL is way performant.

**KRISHNA** Yes, we've spent 50 years optimising the array data-structure and made idiomatic usage highly performant.  

**BRAHMA** Ok, if I use the correct data-structure in the programming language of implementation, may be I can make it performant.  See, earlier I've used Vectors, but now I can try this using arrays in Scala, because in Scala arrays correspond one-to-one to Java arrays. That is, a Scala array Array[Int] is represented as a Java int[], and at the same time I can still have functional API on the Array structure and then compare the two.  Performance characteristics of various data-structures in Scala is given [here](http://docs.scala-lang.org/overviews/collections/performance-characteristics.html).  Also, Array update is a constant-time operation.  

```scala
def primes(until: Int) : Array[Int] = {
  val matrix = Array.ofDim[Int](until, until)
  for {
    i <- 1 to until
    j <- 1 to until
  } yield if (j % i == 0) matrix(i-1)(j-1) = 1 else matrix(i-1)(j-1) = 0
```

**BRAHMA**  Also, instead of doing transpose as a functional array operation, I'll produce a transposed matrix already in one shot.

```scala
def primes(until: Int) : Array[Int] = {
  val matrix = Array.ofDim[Int](until, until)
  for {
    i <- 1 to until
    j <- 1 to until
  } yield if (j % i == 0) matrix(j-1)(i-1) = 1 else matrix(j-1)(i-1) = 0
  
  val summed = matrix.map(_.sum)
  summed.zipWithIndex.collect { case (value, idx) if (value == 2) => idx + 1}
}

def time[T, R](f: Function[T, R]): Function[T, R] = {
  return t => {
    val startTime = System.currentTimeMillis
    val result = f(t)
    val diff = System.currentTimeMillis - startTime
    println(s"Time Taken = $diff(ms).")
    result
  }
}

// println(time(primes)(13)) //12ms, 9ms
// println(time(primes)(130)) //39ms, 51ms
// println(time(primes)(1300)) //264ms, 228ms
println(time(primes)(13000)) //13120 ms, 13384ms
println("Done")

```

**BRAHMA** The performance has increased by around 50%.

**BRAHMA** Also, the documentation throws light on an important caveat:  

> At run-time, when an element of an array of type Array[T] is accessed or updated there is a sequence of type tests that determine the actual array type, followed by the correct array operation on the Java array. These type tests slow down array operations somewhat. You can expect accesses to generic arrays to be three to four times slower than accesses to primitive or object arrays. This means that if you need maximal performance, you should prefer concrete over generic arrays. 

**BRAHMA** Lets park moving to concrete arrays and doing bare bones transformations, instead lets see if I can use available data-structures and make it performant.

```scala
def isPrime(n: Int) = (2::List.range(3,n,2)).takeWhile(Math.pow(_,2) <= n).forall(n % _ > 0)

def primes(n: Int) : List[Int] = (1 to n).filter(isPrime).toList

def time[T, R](f: Function[T, R]): Function[T, R] = {
  return t => {
    val startTime = System.currentTimeMillis  
    val result = f(t)
    val diff = System.currentTimeMillis - startTime
    println(s"Time Taken = $diff(ms).")
    result
  }
} 

// println(time(primes)(13)) //14ms, 13ms
// println(time(primes)(130)) // 29ms, 25ms
// println(time(primes)(1300)) //118ms, 76ms
println(time(primes)(13000))  //1296ms, 1308ms
println("Done")
```

**BRAHMA**  Hmmmmm, very efficient!  Getting closer to APLs performance.  Lets reflect on all this...


Reflections
-----------

**BRAHMA**  In other paradigms, we are working at two things simultaneously - discovering the data-structure and flushing out the logic to work with the scenario.  Once you finish working on that, the next thing is to bring in performance optimisations.  Hence, the order becomes - “make it work, make it better and then make it performant” - becomes a general pattern of improvisation.  However, in APL, you have Arrays as the only data-strucure and so, this causes the logic to be flushed out immediately without worrying about the data-structure discovery.

**KRISHNA** Also, this being the only data-structure, idiomatic usage can be made highly performant.  Less interpreter, more runtime optimisations.

**KRISHNA** In other paradigms, though the optimisations may be present on individual data-structures, but it may not be highly normalised because the designers have a more general purpose use in mind.  

**KRISHNA** Also, I don't know about other platforms, but measuring  performance on the JVM appears to an involved process with many things to be kept in mind.  

**BRAHMA** As I reflect further through this, for example - whether its OO or FP paradigm, a List or Vector data-structure is a generic structure that can hold elements of type T.  It will have to work with type T that satisfy certain type-constraints.  In APL, all you have is Integers and Characters, no Booleans, no Strings or any other data-type.  Does that help in getting performance benefits?  //Please review this Morten

**KRISHNA** //Todo

