def toInt(pair:(Char, Int)) :Int = pair match {
  case (ch, n) => ch.asDigit
}
def luhn(creditCard: String): Boolean = {
  val (odds, evens) = 
    creditCard.reverse.zipWithIndex.partition { 
      case ((ch, n)) => n % 2 == 0 
    }
  val s1 = odds.map(toInt).sum
  val s2 = evens.map(toInt).map(_*2)
            .map(x => if (x < 10) x else (x%10) + 1)
            .sum
  (s1 + s2) toString() endsWith("0")  
}

val creditCards = List("2621195162335", "49927398716", "1234567812345670", "4485284720134093") ++ List("49927398717", "1234567812345678")

println(creditCards filter luhn)
