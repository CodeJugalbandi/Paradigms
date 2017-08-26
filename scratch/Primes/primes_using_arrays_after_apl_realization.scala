def primes(until: Int) : Array[Int] = {
  
// def primes(until: Int) : List[Int] = {
  // val matrix = Array.ofDim[Int](until, until)
  // for {
  //   i <- 1 to until
  //   j <- 1 to until
  // } yield if (j % i == 0) matrix(i-1)(j-1) = 1 else matrix(i-1)(j-1) = 0
  
  // Converting to list as an intermediary causes "out of memory error for 4G of heap"
  // But why convert to List?  Well Arrays are best for updates in constant-time, but not
  // Lists.  If we look at the performance characteristics of various data-structures
  // http://docs.scala-lang.org/overviews/collections/performance-characteristics.html,
  // Lists appear the best choice.  However, it seems that the transpose is causing
  // the issue of out of memory, and thats not the case with transposing the Arrays.
  
  // val summed = matrix.toList.transpose.map(_.sum)
  // summed.zipWithIndex.collect { case (value, idx) if (value == 2) => idx + 1}
  
  
  // Further combining filling and tranpose as one operation reduces the execution
  // time by almost 50% as compared to the above code, where transpose was done based
  // using the available Scala Api.
  
  val transposedMatrix = Array.ofDim[Int](until, until)
  for {
    i <- 1 to until
    j <- 1 to until
  } yield if (j % i == 0) matrix(j-1)(i-1) = 1 else matrix(j-1)(i-1) = 0

  val summed = transposedMatrix.map(_.sum)
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