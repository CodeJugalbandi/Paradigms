// The Problem (Minimal)
// =====================
// Parsing Recursive Structures (Arrays) 
// BNF grammar for Recursive Array Parsing
// value ::= arr | stringLiteral | floatingPointNumber.
// arr ::= "[" [ values ] "]".
// values ::= value {"," value}.
// stringLiteral ::= """\"([^"\p{Cntrl}\\]|\\[\\'"bfnrt]|\\u[a-fA-F0-9]{4})*\""""
// floatingPointNumber ::= "-?(\d+(\.\d*)?|\d*\.\d+)([eE][+-]?\d+)?"

type Parser[T] = String => Option[(T, String)]

// PRIMITIVE PARSERS
def success[T](t: T) : Parser[T] = (in: String) =>
  Some((t, in))
  
def failure = (in: String) => None

def item = (in: String) => in match {
  case "" => None
  case _ => Some((in.head, in.tail))
}

def parse[T](in: String, p: Parser[T]) = p(in)

// PARSER COMBINATORS  
def seq[T, U](p: Parser[T], q: Parser[U]): Parser[(T,U)] = (in: String) => for {
   (x, in1) <- p(in)
   (y, in2) <- q(in1)
  } yield ((x,y), in2)

implicit class ParserExtensions[T](p: Parser[T]) {

  def ~ [U](q: => Parser[U]): Parser[(T,U)] = 
    (in: String) => for {
      (x, in1) <- p(in)
      (y, in2) <- q(in1)
    } yield ((x,y), in2)
    
  // This is flatMap in Scala
  def >>= [U](f: T => Parser[U]): Parser[U] =
    (in: String) => p(in) match {
      case Some((x, in1)) => f(x)(in1)
      case None => None
    }
    
  // def | (q: Parser[T]): Parser[T] = (in: String) =>
  def | [U >: T](q: => Parser[U]): Parser[U] = (in: String) =>
    p(in) match {
      case None => q(in)
      case out @ _ => out
  }

  // This is map in Scala
  def ^^ [U](f: T => U): Parser[U] = 
    p >>= (x => success(f(x)))

  import scala.language.postfixOps
  def * : Parser[List[T]] = 
    p~(p*) ^^ { case (x, xs) => x :: xs } | success(List())
  
  def ~> [U](q: => Parser[U]): Parser[U] = 
    p~q ^^ { case (x,y) => y }
    
  def <~ [U](q: => Parser[U]): Parser[T] = 
    p~q ^^ { case (x,y) => x }
    
  def * (sep: => Parser[Any]): Parser[List[T]] =
    p~(sep~>p*) ^^ {case (x, xs) => x :: xs} | success(List())
}

def satisfy(pred: Char => Boolean): Parser[Char] =
  item >>= (ch => if (pred(ch)) success(ch) else failure)

def char(ch: Char) = satisfy (_ == ch)

def letter = satisfy(Character.isLetter _)

def digit = satisfy(Character.isDigit _)

def alphanum = letter | digit

def singleDigitIntValue: Parser[Int] =
  digit ^^ (ch => Integer.parseInt(ch.toString))
  
def string(s: String): Parser[String] =
  if (s.isEmpty) success("")
  else char(s.head)~string(s.tail) ^^ {case (x,xs) => x + xs}

def many[T](p: Parser[T]) : Parser[List[T]] = 
  p~many(p) ^^ { case (x, xs) => x :: xs } | success(List())

import scala.language.postfixOps
def repsep[T](p: Parser[T], sep: Parser[Any]): Parser[List[T]] =
  p~(sep~>p*) ^^ {case (x, xs) => x :: xs} | success(List())

def floatingPointNumber: Parser[String] =
  regex("""-?(\d+(\.\d*)?|\d*\.\d+)([eE][+-]?\d+)?""".r)

def stringLiteral: Parser[String] = 
  regex("""\"([^"\p{Cntrl}\\]|\\[\\'"bfnrt]|\\u[a-fA-F0-9]{4})*\"""".r)

// def whitespace = regex("""\s+""".r)

def str(s: String): Parser[String] = whitespace~>string(s)
 
def whitespace: Parser[String] = (in:String) => {
  val eatWhitespaceRegex = """\s*""".r
  eatWhitespaceRegex.findPrefixOf(in) match {
    case None => failure(in)
    case Some(wspaces) => success(wspaces)(in diff wspaces)
  }
}

import scala.util.matching.Regex
def regex(r: Regex): Parser[String] = {
  def regexparser = (in:String) => r.findPrefixOf(in) match {
    case None => failure(in)
    case Some(matched) => success(matched)(in diff matched)
  }
  whitespace~>regexparser
}

object ArrayParser {
  // BNF grammar for Array Parsing
  // value ::= arr | stringLiteral | floatingPointNumber
  // arr ::= "[" [ values ] "]".
  // values ::= value {"," value}.
  // stringLiteral ::= """\"([^"\p{Cntrl}\\]|\\[\\'"bfnrt]|\\u[a-fA-F0-9]{4})*\""""
  // floatingPointNumber ::= "-?(\d+(\.\d*)?|\d*\.\d+)([eE][+-]?\d+)?"

  def value = arr | stringLiteral | floatingPointNumber  ^^ (_.toDouble)

  def arr: Parser[List[Any]] = str("[") ~> (value*(str(","))) <~ str("]")

  def apply(in: String) = parse(in, arr)
}

// Sample Array
val eatry = """
[
   "Maratha Pavillion - मराठा पविल्लीयन",
   19.0760, "° N", 72.8777, "° E", "Mumbai, भारत", 
   ["123 456-7890", "987 654-3210"]
]
"""

println(ArrayParser(eatry))
