import scala.collection.parallel.mutable._

def primes(until: Int) : Array[Int] = {
  val transposedMatrix = Array.ofDim[Int](until, until)
  for {
    i <- 1 to until
    j <- 1 to until
  } yield if (j % i == 0) transposedMatrix(j-1)(i-1) = 1 else transposedMatrix(j-1)(i-1) = 0
  val summed = transposedMatrix.par.map(_.sum)
  summed.zipWithIndex.collect { case (value, idx) if (value == 2) => idx + 1}.toArray
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

// println(time(primes)(13) foreach println) //20ms, 18ms
// println(time(primes)(130)) // 67ms, 56ms
// println(time(primes)(1300)) //389ms, 421ms
println(time(primes)(13000)) //26862ms, 25522ms, 31083ms
println("Done")