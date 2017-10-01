// For simplicity our parser reads a raw sequence of characters
// or in other words consumes a String.
// 
// Now parser is just a function from String input type to 
// a parse result.  The type could be written as:
// 
type Parser[T] = String => List[(T, String)]

def success[T](t: T) = (input: String) => List((t, input))

def failure[T] = (input: String) => Nil
// TODO: Introduce failure message
// def failure[T](msg: String = "Failed =>") =
//   (input: String) => List((msg, input))


def item[T] = (input: String) => input match {
    case "" => Nil
    case _ => List((input.head, input.tail))
  }
   
// Parser Combinators
def seq[T, U](p: Parser[T], q: Parser[U]) = (input: String) =>
  for {
     (x, input1) <- p(input)
     (y, input2) <- q(input1)
  } yield ((x,y), input2)

  
  
implicit class ParserExtensions[T](p: Parser[T]) {
  // seq re-written with symbol ~ inside extension class
  def ~[T, U](q: Parser[U]) = (input: String) => 
    for {
      (x, input1) <- p(input)
      (y, input2) <- q(input1)
    } yield ((x,y), input2)
  
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
  // literal string parser
  
}

def satisfy(pred: Char => Boolean) = 
  item >>= (x => if (pred(x)) success(x) else failure)

def char(ch: Char) = satisfy (_ == ch)
def lower = satisfy (Character.isLowerCase _)
def letter = satisfy (Character.isLetter _)
def digit = satisfy (Character.isDigit _)
// def alphanum = satisfy (Character.isLetterOrDigit _)
def alphanum = letter | digit

def string(s: String): Parser[String] = s.toList match {
  case Nil => success("")
  case x::xs => char(x) >>= (_ => (string(xs.mkString) >>= (_ => success((x::xs).mkString))))
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
println((item ~ success(""))("hello"))
println((item >>= (x => success(x)))("hello"))
// println((item >>= (x => failure))("hello"))
// println((failure >>= ((x:Char) => item))("hello"))
// println((success("") >>= (x => item))("hello"))
// println(satisfy(_ == 'L')("Love"))
// println(satisfy(_ == 'L')("love"))
// println("char ->")
// println(char('[')("[]"))
// println(char('a')("bcd"))
// println((char('[') >>= (x => char(']')))("[]"))
// println("lower -> ")
// println(lower("love"))
// println(lower("Love"))
// println("letter -> ")
// println(letter("love"))
// println(letter("Love"))
// println(letter("3ove"))
// println("digit ->")
// println(digit("345"))
// println(digit("h345"))
// println("alphanum -> ")
// println(alphanum("h345"))
// println(alphanum("3hello"))
// println("string ->")
// println(string("hello")("hello"))
// println(string("hello4")("hello4535"))
// println(string("hello")("desi"))
println((string("{") ~ string("}"))("{}"))
println((string("{") >>= (x => string("}")))("{}"))
// value ::= obj | arr | stringLiteral | floatingPointNumber | "null" | "true" | "false".
// obj ::= "{" [ members ] "}".
// arr ::= "[" [ values ] "]".
// members ::= member {"," member}.
// member ::= stringLiteral ":" value.
// values ::= value {"," value}.
//

//  We can minimally implement
// value ::= obj | arr | stringLiteral
// obj ::= "{" [ members ] "}".
// arr ::= "[" [ values ] "]".
// members ::= member {"," member}.
// member ::= stringLiteral ":" value.
// values ::= value {"," value}.

  