// THE PARSER TYPE
// ===============
//
// So to formalize a Parser type, it would be a function that
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
  // chain the unparsed input of the first parser to the second 
  // and return the result of first and second parser as a pair and
  // the final residual unparsed input
  def ~[T, U](q: => Parser[U]) = (input: String) => 
    for {
      (x, input1) <- p(input)
      (y, input2) <- q(input1)
    } yield ((x,y), input2)

  // chain the output of first parser to second and return
  // the result of second parser and residual input.
  // flatMap in Scala
  def >>=[U](f: T => Parser[U]): Parser[U] = 
    (input: String) => 
      (for {
        (x, input1) <- p(input)
      } yield f(x)(input1)).flatten

  // Alternative composition
  def | [U >: T](q: => Parser[U]): Parser[U] = 
    (input: String) => p(input) match {
      case List() => q(input)
      case result => result
  }
  
  // Result Conversion: map in Scala
  def ^^[U](f: T => U): Parser[U] = p >>= (x => success(f(x)))
  
  // other sequential composition operators
  // return right result, ignore left
  def ~> [U](q: => Parser[U]) = (p~q) ^^ { case (x,y) => y }
  // return left result, ignore right
  def <~ [U](q: => Parser[U]) = (p~q) ^^ { case (x,y) => x }

  //optional parser: convert to Option[T].
  def ? : Parser[Option[T]] = p ^^ (Option(_)) | success(None)
  
  import scala.language.postfixOps
  def * : Parser[List[T]] = 
    // (p~(p*) ^^ { case(x, xs) => x::xs })(input)
    (p~(p*) ^^ { case (x,xs) => x::xs } | success(List()))

  // Sometimes we are interested in non-empty sequences of items.
  // So, we define a + combinator, in terms of *
  def + : Parser[List[T]] = (p~(p*) ^^ { case (x,xs) => x::xs })
  
  // many parsing with seperator
  def * (sep: Parser[Any]) : Parser[List[T]] = 
    (p~((sep~>p)*) ^^ { case (x,xs) => x::xs }|success(List()))
    
  // many1 parsing with seperator
  def + (sep: Parser[Any]) : Parser[List[T]] = 
    (p~((sep~>p)+) ^^ { case (x,xs) => x::xs }|success(List()))
}

def satisfy(pred: Char => Boolean) = 
  item >>= (x => if (pred(x)) success(x) else failure)

def char(ch: Char) = satisfy ((x:Char) => x == ch)
def letter = satisfy (Character.isLetter _)
def digit = satisfy (Character.isDigit _)
// def alphanum = satisfy (Character.isLetterOrDigit _)
def alphanum = letter | digit

def string(s: String): Parser[String] = s.toList match {
  case List() => success("")
  case x::xs =>
    ((char(x)~string(xs.mkString)) >>= (_ => success((x::xs).mkString)))
}
  
// def string(s: String): Parser[String] = s.toList match {
//   case Nil => success("")
//   case x::xs => char(x) >>= (_ => (string(xs.mkString) >>= (_ =>success((x::xs).mkString))))
// }

import scala.util.matching.Regex
def regex(r: Regex): Parser[String] = {
  val regexparser = ((input: String) => r.findPrefixOf(input) match {
    case Some(matched) => success(matched)(input diff matched)
    case None => failure(input)
  })
  (whitespace~regexparser) ^^ { case(wspaces, useful) => useful }
}

// import scala.util.matching.Regex
// def regex(r: Regex): Parser[String] =
//   (input: String) => r.findPrefixOf(input) match {
//     case Some(matched) => success(matched)(input diff matched)
//     case None => failure(input)
//   }

def stringLiteral : Parser[String] = {
  val e = """\"([^"\p{Cntrl}\\]|\\[\\'"bfnrt]|\\u[a-fA-F0-9]{4})*\"""".r
    regex(e)
}

def floatingPointNumber : Parser[String] = regex("[-|+]?(\\d+(\\.\\d*)?|\\d*\\.\\d+)([eE][+-]?\\d+)?".r)


// defining whitespace without regex
def whitespace : Parser[String] = (input: String) => {
  val pattern = "\\s*".r
  pattern.findPrefixOf(input) match {
    case Some(wspaces) => List((wspaces,input diff wspaces))
    case _ => List()
  }
}
// defining whitespace in-terms of regex
// def whitespace = regex("\\s+".r)

// def word : Parser[String] = ((letter~word) ^^ {case (x, xs) => x + xs } | success(""))

// Re-defining word in terms of +
import scala.language.postfixOps
def word: Parser[String] = (letter+) ^^ { case chs => chs.mkString }
   
def integer : Parser[Int] = 
  ((char('-')?) ^^ { 
    case Some(x) => Math.negateExact _
    case None => identity[Int] _ 
  }) >>= ((fn => (digit+) ^^ (chs => fn(Integer.parseInt(chs.mkString)))))
 
def _null = string("null")

def _true = string("true")

def _false = string("false")

  
def str(in: String): Parser[String] = 
  whitespace~string(in) ^^ { case (wspaces, useful) => useful }

def parse[T](inp: String, p: Parser[T]): Any = p(inp) match {
  case Nil => s"Parsing Failed! => $inp"
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

// ParserExtensions _
// println("Using Parser Extensions...")
// println((item ~ failure)("hello"))
// println((failure ~ item)("hello"))
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
// println(string("hello")("  hello4535"))
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
// println("return a ~ b ~ c")
// println(parse("[3]", ((char('[') ~ digit ~ char(']')) ^^ (_.toString))))

// println("optional parser ->")
// println(letter?("345"))
// println(letter?("abc"))
// println(digit?("345"))
// println(digit?("abc"))

// println("word ->")
// println(word(""))
// println(word("a"))
// println(word("an"))
// println(word("Hello5 world"))
// println("* (many - zero or more) parser ->")
// println(letter*(""))
// println(letter*("abc567"))
// println(digit*("345abc"))
// println("+ (many1 - one or more) parser ->")
// println(letter+(""))
// println(letter+("abc567"))
// println(digit+("345abc"))
// println("* with seperator (many - zero or more) parser ->")
// println((letter*(string(",")))("abc"))
// println((letter*(string(",")))("a,b,cdefg"))
// println("+ with seperator (many1 - one or more) parser ->")
// println((letter+(string(",")))("abc"))
// println((letter+(string(",")))("a,b,cdefg"))
// println("integer ->")
// println(integer("12345"))
// println(integer("-123"))
// println("whitespace ->")
// println(whitespace("\n\r\t \b"))
// println(whitespace(""))
// println("composing parser for parsing integers ->")
// println((char('[') ~> (integer*(char(','))) <~ char(']'))("[1,2,3,4,5,6,7,8,9,10]"))
// println(((stringLiteral~(string(":")~stringLiteral)) ^^ { case (k,(_,v)) => (k,v) })(""""name":"test""""))
// println(((stringLiteral~(str(":")~stringLiteral)) ^^ { case (k,(_,v)) => (k,v) })("""  "name":"test""""))

// BNF notation for JSON
// value ::= obj | arr | stringLiteral | floatingPointNumber | "null" | "true" | "false".
// obj ::= "{" [ members ] "}".
// members ::= member {"," member}.
// member ::= stringLiteral ":" value.
// arr ::= "[" [ values ] "]".
// values ::= value {"," value}.
// stringLiteral ::= """[\"|']([^"\p{Cntrl}\\]|\\[\\'"bfnrt]|\\u[a-fA-F0-9]{4})*[\"|']"""
// floatingPointNumber ::= "-?(\d+(\.\d*)?|\d*\.\d+)([eE][+-]?\d+)?"


object ParseJSON {
  def obj : Parser[Map[String, Any]] =
    (str("{") ~> (member*(str(","))) <~ str("}")) ^^ (Map() ++ _)

  def arr : Parser[List[Any]] =
    str("[") ~> (value*(str(","))) <~ str("]")
    
  def member : Parser[(String, Any)] =
    (stringLiteral ~ str(":") ~ value) ^^ { case((k,_),v) => (k,v) }
    
  def value : Parser[Any] = 
     (obj | arr | stringLiteral | (floatingPointNumber ^^ (_.toDouble)) | (_null ^^ (_ => null)) | (_true ^^ (_ => true)) | (_false  ^^ (_ => false)))
     
  def apply(input: String) = parse(input, obj)
}

// println(ParseJSON("""\"hello dhaval\""""))
// println(ParseJSON("[2,3,4]"))
// println(ParseJSON("['hello', 3, 4]"))
// println(ParseJSON("""  "name"  :  "test"  \n"""))
// println(ParseJSON("""{"name":"test"}"""))
// println(ParseJSON("""{"name":null}"""))
// println(ParseJSON("""{"name":true}"""))
// println(ParseJSON("""{"name":false}"""))
// println(ParseJSON("""{"name":23}"""))
// println(ParseJSON("""{"name":{}}"""))
// println(ParseJSON("""{"name":[]}"""))
// println(ParseJSON("""["hello"]"""))
// We can minimally implement
// value ::= obj | arr | stringLiteral
// obj ::= "{" [ members ] "}".
// arr ::= "[" [ values ] "]".
// members ::= member {"," member}.
// member ::= stringLiteral ":" value.
// values ::= value {"," value}.
// stringLiteral ::= """[\"|']([^"\p{Cntrl}\\]|\\[\\'"bfnrt]|\\u[a-fA-F0-9]{4})*[\"|']"""

val sampleJSON = """
{
  "contact": {
    "name": "First Last",
    "email" : "अध्यनाम.उपनाम@पृथ्वी.अाकाशगंगा",
    "address": {
      "street": "10 Market Road",
      "city"  : "Mumbai, MH",
      "country" : "भारत",
      "zip"   : 555555
    },
    "phone numbers": [
      {"home" : "123 456-7890"},
      {"office" : "987 654-3210"},
      {"cell" : "222 444-6666"}
    ]
  }
}
"""
println(ParseJSON(sampleJSON))
  