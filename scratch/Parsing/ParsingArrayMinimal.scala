// BNF (Backus-Naur Form) grammar for Array Parsing
// value ::= arr | letter | digit
// arr ::= "[" [ values ] "]".
// values ::= value {"," value}.
// 
// NOTE: For simplicity, we will not consider whitespaces to be parsed.
// 
// 
// 1. Show minimalist code to start with
// 2. Take one step to demo value by extension of feature
//    - repetition
//    - nested (recursive)
// evolve the same in APL.  highlight the difference in 
// extensibility with functional code and its pros and cons
// with APL code extensibility.

type Parser[T] = String => Option[(T, String)]  

// PRIMITIVE PARSERS
def success[T](t: T): Parser[T] = (in: String) => Some((t, in))
  
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
  // Moved and Renamed `seq` to symbol `~` 
  def ~ [U](q: => Parser[U]): Parser[(T,U)] = 
    (in: String) => for {
      (x, in1) <- p(in)
      (y, in2) <- q(in1)
    } yield ((x,y), in2)
    
  // This is flatMap in Scala.
  def >>= [U](f: T => Parser[U]): Parser[U] =
    (in: String) => p(in) match {
      case Some((x, in1)) => f(x)(in1)
      case None => None
    }

  // This is map in Scala.
  def ^^ [U](f: T => U): Parser[U] = 
    p >>= (x => success(f(x)))
    
  def | [U >: T](q: => Parser[U]): Parser[U] = (in: String) =>
    p(in) match {
      case None => q(in)
      case out @ _ => out
  }
  // Moved and renamed `many` to `*`
  import scala.language.postfixOps
  def * : Parser[List[T]] = 
    p~(p*) ^^ { case (x, xs) => x :: xs } | success(List())
  // Right
  def ~> [U](q: => Parser[U]): Parser[U] = 
    p~q ^^ { case (x,y) => y }
  // Left  
  def <~ [U](q: => Parser[U]): Parser[T] = 
    p~q ^^ { case (x,y) => x }
  // Moved and renamed `repsep` to `*`  
  def * (sep: => Parser[Any]): Parser[List[T]] =
    p~(sep~>p*) ^^ {case (x, xs) => x :: xs} | success(List())
}

def satisfy(pred: Char => Boolean): Parser[Char] =
  item >>= (ch => if (pred(ch)) success(ch) else failure)

def char(ch: Char): Parser[Char] = satisfy (_ == ch)

def letter: Parser[Char] = satisfy(Character.isLetter _)

def digit: Parser[Char] = satisfy(Character.isDigit _)

def digitAsInt: Parser[Int] =
  digit ^^ (ch => Integer.parseInt(ch.toString))

def alphanum: Parser[Any] = letter | digitAsInt

def word: Parser[String] =
  letter~word ^^ { case (x,xs) => x + xs } | success("")

// Generalizing word to many.
def many[T](p: Parser[T]) : Parser[List[T]] = 
  p~many(p) ^^ { case (x, xs) => x :: xs } | success(List())

import scala.language.postfixOps
def repsep[T](p: Parser[T], sep: Parser[Any]): Parser[List[T]] =
  p~(sep~>p*) ^^ {case (x, xs) => x :: xs} | success(List())

object ArrayParser {
  // BNF grammar for Array Parsing
  // value ::= arr | letter | digit
  // arr ::= "[" [ values ] "]".
  // values ::= value {"," value}.
  def value = arr | letter | digitAsInt

  def arr: Parser[List[Any]] = 
    // char('[') ~> (value*) <~ char(']')
    char('[') ~> (value*(char(','))) <~ char(']')
    
  def apply(in: String) = parse(in, arr)
}
// val sample = "[a]"
// val sample = "[a1]"
// val sample = "[a,1]"
// val sample = "[a,[1]]"
val sample = "[a,1,[2,b]]"
println(ArrayParser(sample))