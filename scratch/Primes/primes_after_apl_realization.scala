def primes(until: Int) : List[Int] = {
  val vector = for {
    i <- 1 to until
    j <- 1 to until
  } yield if (j % i == 0) 1 else 0

  val matrix = vector.grouped(until).toVector.transpose
  val summed = matrix.map(_.sum).toList
  summed.zip(Stream.from(1)).collect { case (value, idx) if (value == 2) => idx}
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