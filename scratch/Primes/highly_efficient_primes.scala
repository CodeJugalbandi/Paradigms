// Very slow prime 
// def isPrime(n: Int) = (2 to n).filter(x => n % x == 0) == List(n)

// Highly efficient prime
def isPrime(n: Int) = (2::List.range(3,n,2)).takeWhile(Math.pow(_,2) <= n).forall(n%_ > 0)

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