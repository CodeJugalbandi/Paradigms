def primes(until: Int) : List[Int] = {
  val vector = for {
    i <- 1 to until
    j <- 1 to until
  } yield if (j % i == 0) 1 else 0

  val matrix = vector.grouped(until).toVector.transpose
  val summed = matrix.map(_.sum).toList
  summed.zip(Stream.from(1)).collect { case (value, idx) if (value == 2) => idx}
}

println(primes(13))