
def toFizzBuzz(n: Int) = {
    if (n.toString contains "3") "lucky"
    else if (n % 15 == 0) "fizzbuzz"
    else if (n % 3 == 0) "fizz"
    else if (n % 5 == 0) "buzz"
    else n
}                                             
  
val fizzBuzzed = (1 to 20) map toFizzBuzz     
       
println(fizzBuzzed)

