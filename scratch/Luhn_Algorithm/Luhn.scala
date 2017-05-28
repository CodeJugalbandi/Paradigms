def luhn(creditCard: String): Boolean = {
  val (odds, evens) = creditCard.reverse.zip(Range(1, creditCard.size + 1)).partition { case ((ch, n)) => n % 2 != 0 }
  val s1 = odds.map { case ((ch, _)) => ch.asDigit }.sum
  val s2 = evens.map { case ((ch, _)) => {     
    val doubled = ch.asDigit * 2 
    if (doubled < 10) doubled else doubled % 10 + 1
  }}.sum
  (s1 + s2) toString() endsWith("0")  
}

val (validNumbers, invalidNumbers) =
    (List("2621195162335", "49927398716", "1234567812345670", "4485284720134093"),
      List("49927398717", "1234567812345678"))

val creditCards = validNumbers ++ invalidNumbers
println(creditCards filter luhn)
