// THE PARSER TYPE
// ===============
//
// So to formalize a parser type, it would be a function that
// consumes a string and returns a Tree.  In a functional language
// such as Scala, parsers can naturally be viewed as functions.
//
// type Parser = String -> Tree
//
// So essentially a parser is a function that takes a string
// and returns some form of tree.
//
// However, in reality a parser might not require all of its
// input string, so we also return any unused input, so lets
// generalize the parser as consuming string and returning
// a pair of Tree and String:
//
// type Parser = String -> (Tree, String)
//
// Further, a given string can be parsed in many ways, if
// the grammar is ambiguous this can happen.  Also, its
// quite possible that the string cannot be parsed into a
// Tree.  So, in such cases, we need to return a list of
// results like this:
//
// type Parser = String -> [(Tree, String)]
//
// so, that list can be empty when there is nothing to parse,
// or if it is ambiguous, it can contain many pairs or if it
// is a non-ambiguous grammar, it can be a singleton list (a
// list containing one element) or an empty list.
//
// Summarizing:
// 1. Parsing Fails -> Empty List.
// 2. Parsing Succeeds -> Singleton List with Pair.
// 3. Ambiguous Grammar -> List with Pairs of elements.
//
// Finally, a parser might not always produce a tree, so we
// generalize to a value of any type using type parameter.  
// For simplicity our parser reads a raw sequence of characters
// or in other words consumes a String.
// 
// So parser is just a function from String input type to 
// a parse result.  The type could be written as:
// 
type Parser[T] = String => List[(T, String)]

def success[T](t: T) = (input: String) => List((t, input))

def failure[T] = (input: String) => List()
// TODO: Introduce failure message
// def failure[T](msg: String = "Failed =>") =
//   (input: String) => List((msg, input))

// single item parser
def item[T] = (input: String) => input match {
    case "" => List()
    case _ => List((input.head, input.tail))
  }
   
// Parser Combinators
def seq[T, U](p: Parser[T], q: Parser[U]) = (input: String) =>
  for {
     (x, input1) <- p(input)
     (y, input2) <- q(input1)
  } yield ((x,y), input2)

  
// Parser Combinators  
implicit class ParserExtensions[T](p: Parser[T]) {

  // seq re-written with symbol ~ inside extension class
  def ~[T, U](q: Parser[U]) = (input: String) => 
    for {
      (x, input1) <- p(input)
      (y, input2) <- q(input1)
    } yield ((x,y), input2)

  
  // chain output of first parser to second and return
  // the result of second parser and remaining input.
  // flatMap in Scala
  def >>=[U](f: T => Parser[U]): Parser[U] = 
    (input: String) => 
      (for {
        (x, input1) <- p(input)
      } yield f(x)(input1)).flatten


  // Alternative composition
  def | (q: Parser[T]): Parser[T] = 
    (input: String) => p(input) match {
      case Nil => q(input)
      case result => result
  }
  
  // Result Conversion
  // map in Scala
  def ^^[U](f: T => U): Parser[U] = p >>= (x => success(f(x)))
  
  // other sequential composition operators
  // return right result, ignore left
  def ~> [U](q: Parser[U]) = (p~q) ^^ { case (x,y) => y }
  // return left result, ignore right
  def <~ [U](q: Parser[U]) = (p~q) ^^ { case (x,y) => x }

  // def flatMap = >>= _
  // def map = ^^ _
  
  //optional parser: convert to Option[T].
  def ? : Parser[Option[T]] = p ^^ {
    case List() => None
    case x => Some(x)
  }
  
  import scala.language.postfixOps
  def * : Parser[List[T]] = (input: String) => 
    // (p~(p*) ^^ { case(x, xs) => x::xs })(input)
    (p~(p*) ^^ { case (x,xs) => x::xs } | success(List()))(input)

 
  // Sometimes we are interested in non-empty sequences of items.
  // So, we define a + combinator, in terms of *
  def + : Parser[List[T]] = (input: String) =>
    (p~(p*) ^^ { case (x,xs) => x::xs })(input)
  
  // many parsing with seperator
  def * (sep: Parser[Any]) : Parser[List[T]] = (input: String) =>
    (p~((sep~>p)*) ^^ { case (x,xs) => x::xs }|success(List()))(input) 
    
  // many1 parsing with seperator
  def + (sep: Parser[Any]) : Parser[List[T]] = (input: String) =>
    (p~((sep~>p)+) ^^ { case (x,xs) => x::xs }|success(List()))(input) 
 
}

def satisfy(pred: Char => Boolean) = 
  item >>= (x => if (pred(x)) success(x) else failure)

def char(ch: Char) = satisfy ((x:Char) => x == ch)
def letter = satisfy (Character.isLetter _)
def digit = satisfy (Character.isDigit _)
// def alphanum = satisfy (Character.isLetterOrDigit _)
def alphanum = letter | digit

def string(s: String): Parser[String] = s.toList match {
  case Nil => success("")
  case x::xs => char(x) >>= (_ => (string(xs.mkString) >>= (_ => success((x::xs).mkString))))
}

import scala.util.matching.Regex
def regex(r: Regex): Parser[String] =
  (input: String) => r.findPrefixOf(input) match {
    case Some(matched) => success(matched)(input diff matched)
    case None => failure(input)
  }
  
def stringLiteral = {
  val e = """[\"|']([^"\p{Cntrl}\\]|\\[\\'"bfnrt]|\\u[a-fA-F0-9]{4})*[\"|']""".r
    regex(e)
}


def floatingPointNumber = regex("-?(\\d+(\\.\\d*)?|\\d*\\.\\d+)([eE][+-]?\\d+)?".r)

// def word : Parser[String] = (input: String) =>
//   ((letter~word) ^^ {case (x, xs) => x + xs } | success(""))(input)

// Re-defining word in terms of *
import scala.language.postfixOps
def word: Parser[String] = (letter*) ^^ { case chs => chs.mkString }
   

def parse[T](inp: String, p: Parser[T]): Any = p(inp) match {
  case Nil => Nil
  case List((x, _)) => x
  case results => results
}

// println(success("d")("Hello"))
// println(success("")("hello"))
// println(failure("hello"))
// println(failure("hello"))
// println(item(""))
// println(item("hello"))
// println(seq(item, failure)("hello"))
// println(seq(item, success(""))("hello"))

ParserExtensions _
// println("Using Parser Extensions...")
// println((item ~ failure)("hello"))
// println((item ~ success(""))("hello"))
// println((item >>= (x => success(x)))("hello"))
// println((item >>= (x => failure))("hello"))
// println((failure >>= ((x:Char) => item))("hello"))
// println((success("") >>= (x => item))("hello"))
// println(satisfy(_ == 'L')("Love"))
// println(satisfy(_ == 'L')("love"))
// println("char ->")
// println(char('[')("[]"))
// println(char('a')("bcd"))
// println((char('[') >>= (x => char(']')))("[]"))
// println("letter -> ")
// println(letter("love"))
// println(letter("Love"))
// println(letter("3ove"))
// println("digit ->")
// println(digit("345"))
// println(digit("h345"))
// println("Oring ->")
// println((success("success") | failure)("10"))
// println(parse("10", failure | success("success")))
// println("alphanum -> ")
// println(alphanum("h345"))
// println(alphanum("3hello"))
// println("string ->")
// println(string("hello")("hello"))
// println(string("hello4")("hello4535"))
// println(string("hello")("desi"))
// println("map ->")
// println((digit ^^ (x => Integer.parseInt(x.toString)))("345"))
// println(((char('[') ~ char(']')) ^^  { case (x,y) => Nil })("[]"))
// println((string("{") ~ string("}"))("{}"))
// println((string("{") >>= (x => string("}")))("{}"))
// println("regex parsers ->")
// println(regex("Scala".r)("ScalaScala is Scalable"))
// println("stringLiteral ->")
// println(parse("\"अध्यनाम उपनाम\"", stringLiteral))
// println(parse("'अध्यनाम.उपनाम@पृथ्वी.अाकाशगंगा'", stringLiteral))
// println(parse("10e-2", (floatingPointNumber ^^ (java.lang.Float.parseFloat(_)))))
// println("return left <~")
// println(parse("a}", (letter <~ string("}")) ^^ (ch => ch.toString)))
// println("return ~> right ")
// println(parse("{b' ", (string("{") ~> letter) ^^ (ch => ch.toString)))
// println("return ~> center <~")
// println(parse("[3]", ((char('[') ~> digit <~ char(']')) ^^ (List(_)))))
// println("optional parser ->")
// println(letter?("345"))
// println(letter?("abc"))
// println(digit?("345"))
// println(digit?("abc"))

println("word ->")
println(word("Hello5 world"))
println("* (many - zero or more) parser ->")
println(letter*(""))
println(letter*("abc567"))
println(digit*("345abc"))
println("+ (many1 - one or more) parser ->")
println(letter+(""))
println(letter+("abc567"))
println(digit+("345abc"))
println("* with seperator (many - zero or more) parser ->")
println((letter*(string(",")))("abc"))
println((letter*(string(",")))("a,b,cdefg"))
println("+ with seperator (many1 - one or more) parser ->")
println((letter+(string(",")))("abc"))
println((letter+(string(",")))("a,b,cdefg"))


// BNF notation for JSON
// value ::= obj | arr | stringLiteral | floatingPointNumber | "null" | "true" | "false".
// obj ::= "{" [ members ] "}".
// members ::= member {"," member}.
// member ::= stringLiteral ":" value.
// arr ::= "[" [ values ] "]".
// values ::= value {"," value}.
// stringLiteral ::= """[\"|']([^"\p{Cntrl}\\]|\\[\\'"bfnrt]|\\u[a-fA-F0-9]{4})*[\"|']"""
// floatingPointNumber ::= "-?(\d+(\.\d*)?|\d*\.\d+)([eE][+-]?\d+)?"


// We can minimally implement
// value ::= obj | arr | stringLiteral
// obj ::= "{" [ members ] "}".
// arr ::= "[" [ values ] "]".
// members ::= member {"," member}.
// member ::= stringLiteral ":" value.
// values ::= value {"," value}.
// stringLiteral ::= """[\"|']([^"\p{Cntrl}\\]|\\[\\'"bfnrt]|\\u[a-fA-F0-9]{4})*[\"|']"""

//  Sample JSON
// {
//   "contact": {
//     "name": "First Last",
//     "email" : "अध्यनाम.उपनाम@पृथ्वी.अाकाशगंगा",
//     "address": {
//       "street": "10 Market Road",
//       "city"  : "Mumbai, MH",
//       "country" : "भारत",
//       "zip"   : 555555
//     },
//     "phone numbers": [
//       {"home" : "123 456-7890"},
//       {"office" : "987 654-3210"},
//       {"cell" : "222 444-6666"}
//     ]
//   }
// }
 
  