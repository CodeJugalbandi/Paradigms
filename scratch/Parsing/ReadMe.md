#Parsing Text Containing Recursive Structures

Let us consider the problem of parsing arrays.  Examples could be:

* An empty array - ```"[]"```, 
* An array with single letter - ```"[a]"``` or a single digit - ```"[1]"```,
* An heterogeneous array with a letter and a digit - ```"[a,1]"```,
* Nested Arrays - ```"[a,[1]]", "[अ,१,[२,ब]]"```

The BNF (Backus-Naur Form) grammar for Arrays containing letters, digits and nested arrays can be described as:

```
value ::= arr | letter | digit.
arr ::= "[" [ values ] "]".
values ::= value {"," value}.
```

**_NOTE_**: For simplicity, we will not consider whitespaces to be parsed.
 

Code Jugalbandi in Functional Programming & Array-Oriented Paradigms
----

**BRAHMA** Lets look at implementing this in APL.

**BRAHMA** I'll approach this in 3 parts, first make sure the parsing of digits and letter happens, then look at parsing multiple elements and finally look at parsing nested arrays.

```apl
⍝ TODO
```

**BRAHMA** Krishna, how does this look in OO or Functional paradigm?

**KRISHNA** Ok, let me show you how this can be approached in a functional programming paradigm.  I'll use Scala once again.  We can visualize  parser as a function that consumes a ```String``` and produces a structure of type ```T```.  Formalizing the parser type:

```scala
type Parser[T] = String => T
```

**KRISHNA**  However, in reality a parser may not consume all the input string, so we return the unused string.  So lets change the return type to a tuple in order to accomodate the unused string:

```scala
type Parser[T] = String => (T, String)
```

**KRISHNA** Also, the parser may or may not be able to parse the input string.  So, it either the produces the desired structure ```T``` or it does not.  Lets make this absence or presence explicit in the Parser return type using ```Option```:

```scala
type Parser[T] = String => Option[(T, String)]
```

**KRISHNA** Having defined parser type, we can now start implementing parsers.  The way we approach this is - we start with simple parsers and then build complicated parsers out of simple parsers.  An example of simple parser is one that consumes a single character from the string and returns that character along with the unused string.

```scala
def item: Parser[Char] = (in: String) => in match {
  case "" => None
  case _  => Some((in.head, in.tail))
}

println(item("hello")) // Some(h,ello)
println(item(""))      // None
```

**KRISHNA** Now lets write another simple parser that always fails.

```scala
def failure = (in: String) => None

println(failure("hello")) // None
println(failure(""))      // None
```

**KRISHNA** We now define a dual of the above parser, i.e., a parser that always succeeds.  Whatever value we feed to it, it returns the same value back, without consuming any input.

```scala
def success[T](t: T): Parser[T] = 
  (in: String) => Some((t, in))
  
println(success(1)("world"))       // Some((1, "world"))
println(success("hello")("world")) // Some(("hello", "world"))
```

**KRISHNA** Now, that we have these primitive parsers, we can start building complex parsers  by combining them.  Taking cues from BNF grammar - a language for defining a language, wherein it composes bigger grammar from smaller grammer. BNF grammar for an array can be given as:

```
value ::= array | letter | digit
array ::= "[" [ values ] "]".
values ::= value {"," value}.
```

**KRISHNA** In that light, lets define a sequential parser that consumes two parsers and produces another parser, thus combining the two parsers.  Operationally, the first parser in the sequence consumes the input and feeds its unparsed output as input to the second parser.  The sequence combinator thus applies the two parsers one after the other and returns the results of the two parsers as a pair along with the residual output from the second parser.

```scala
def seq[T, U](p: Parser[T], q: Parser[U]): Parser[(T,U)] = 
  (in: String) => for {
      (x, in1) <- p(in)
      (y, in2) <- q(in1)
  } yield ((x,y), in2)

println(seq(item, success("hello"))("world")) // Some(((w,hello),orld))
println(seq(success("hello"), item)("world")) // Some(((hello,w),orld))
println(seq(item, failure)("world")) // None
```

**KRISHNA** Now again, in effort to go closer to  BNF symbols used for defining grammar, i'll make `seq` sequential composition an infix operator, so that its possible to translate BNF grammar to a parser specification without much effort.  For this I'll resort to Scala's ```implicit class``` mechanism and move the ```seq``` method inside.   Scala requires that an implicit class must have a constructor that takes in a parameter on which the operator can be invoked (in other words a receiver).  So the first parameter ```p: Parser[T]``` goes in as constructor parameter and the second remains with the method itself.  I'll also rename ```seq``` to symbol ```~```, here is how it looks:

```scala
implicit class ParserExtensions[T](p: Parser[T]) {
  def ~ [U](q: Parser[U]): Parser[(T,U)] = 
    (in: String) => for {
      (x, in1) <- p(in)
      (y, in2) <- q(in1)
    } yield ((x,y),in2)
}
```

**KRISHNA** For convenience, I'll also define a ```parse``` function, like this:

```scala
def parse[T](in: String, p: Parser[T]) = p(in)

println(parse("world", item ~ success("hello"))) // Some(((w,hello),orld))
println(parse("world", success("hello") ~ item)) // Some(((hello,w),orld))
println(parse("world", item ~ failure)) // None
```

**KRISHNA** Many a times, we simply don't want to have the nested tuples in the output like the above, its messy. We would rather not return the output of the first parser, instead simply chain that output as input to the second parser in sequence.  I'll then return the output of the second parser - which comprises of its actual output value and residual input after both the parsers have done their job.  To enable chaining, in place of the second parameter ```q: Parser[U]```, i'll install a function that consumes the output produced by the first parser and returns the second parser.  We define another operator called ```chain``` or ```flatMap``` as generally named in Scala.   Haskell terminology uses ```bind``` or ```shove``` for it.  For this, i'll use the Haskell symbol ```>>=``` and define it within ParserExtensions so that its available in its infix form.  

```scala
implicit class ParserExtensions[T](p: Parser[T]) {
  def ~ [U](q: Parser[U]): Parser[(T,U)] = 
    (in: String) => for {
      (x, in1) <- p(in)
      (y, in2) <- q(in1)
    } yield ((x,y),in2)
    
  def >>= [U](f: T => Parser[U]): Parser[U] = 
    (in: String) => for {
      (x, in1) <- p(in)
      (y, in2) <- f(x)(in1)
    } yield (y, in2)
}

println(parse("world", item >>= (x => success(x)))) // Some((w,orld))
println(parse("world", item >>= (x => failure))) // None
```
**KRISHNA**  This is called monadic sequencing, where the nest is removed, whereas the earlier combinator ```~``` is plain nested sequencing. So just like the earlier operator, if any one parser in the sequence fails, the entire sequence fails and if all parsers succeed, then the entire sequence succeeds.

**KRISHNA** Using the above definition, I can now define another simple and a useful parser that consumes a character from input and checks that against a given predicate.  If the predicate is satisfied, then it returns that character else it simply fails.  Lets call this character parser ```satisfy```.  

```scala
def satisfy(pred: Char => Boolean): Parser[Char] = ???
```

**KRISHNA** If we recall, the earlier ```item``` parser consumed a character without any reservations, so we will use that and chain its output and apply the predicate.  If the predicate fails, return a failure parser and if it succeeds, return the accepted character in a success parser along with the residual input.

```scala
def satisfy(pred: Char => Boolean): Parser[Char] = 
  item >>= (ch => if (pred(ch)) success(ch) else failure)
    
println(parse("hello", satisfy(_ == 'h'))) // Some((h,ello))
println(parse("world", satisfy(_ == 'h'))) //
None
println(parse("", satisfy(_ == 'h'))) //
None
```

**KRISHNA** Using the satisfy parser, I can now build a parser that consumes a specific character from the input string and rejects everything else:

```scala
def char(ch: Char) = satisfy(ch == _)

println(parse("[]", char('['))) // Some(([,]))
println(parse("[]", char('!'))) // None
```

**KRISHNA** Likewise, using the satisfy parser, I can also build a letter and a digit parser:

```scala
def letter = satisfy(Character.isLetter _)

println(parse("hello", letter)) // Some((h,ello))
println(parse("12345", letter)) // None

def digit = satisfy(Character.isDigit _)

println(parse("12345", digit)) // Some((1,2345))
println(parse("hello", digit)) // None
```

**KRISHNA** Let me now create an alphanumeric parser that consumes either a letter or digit:

```scala
def alphanum = letter | digit
```

**KRISHNA** But I don't have the choice ```|``` combinator, so I'll create one.  The choice combinator applies the first parser, if ```p``` fails, it tries the second parser ```q``` on the same input (allows back-tracking) and returns the output of the second parser.  In case the first succeeds, it returns the output from the first and does not evaluate the second.

```scala
implicit class ParserExtensions[T](p: Parser[T]) {
  ...
  ...    
  def | [U >: T](q: Parser[U]): Parser[U] = 
    (in: String) => p(in) match {
      case None => q(in)
      case out  => out
    }
}

println(parse("a1", alphanum)) // Some((a,1))
println(parse("1a", alphanum)) // Some((1,a))
println(parse("!!", alphanum)) // None
```

**KRISHNA** Now, if you look the digit parser produces a digit as a character, however we really want it to be an integer to be able to something useful with it. So lets introduce a mapping operation on the parser that consumes a function which maps the input to the required output type.  I'll use the symbol ```^^``` double caret for that.

```scala
implicit class ParserExtensions[T](p: Parser[T]) {
  ...    
  ...
  def ^^ [U](f: T => U): Parser[U] = 
    p >>= (t => success(f(t)))
}
```

**KRISHNA** The above ```map``` is implemented in terms of ```bind``` or```flatMap```.  If ```p``` succeeds, then the value is wrapped in a ```success``` parser. I can now use the above ```digit``` parser as:

```scala
def digitAsInt: Parser[Int] = digit ^^ (ch => Integer.parseInt(ch.toString))

println(parse("3", digitAsInt)) // Some((3,)) type: Option[(Int, String)]
println(parse("a", digitAsInt)) // None       type: Option[(Int, String)]
```

**KRISHNA** Now, I can parse an array with single element like this:

```scala
println(parse("[a]", char('[') ~ (letter | digitAsInt) ~ char(']'))) // Some(((([,a),]),))
```

**KRISHNA** If you look at the output, I'm really not interested in accumulating the square brackets, I'd like to drop them, so that I can simply extract the value.  For this, I'll write a drop left and drop right parsers.

```scala
implicit class ParserExtensions[T](p: Parser[T]) {
  ...    
  ...
  def ~> [U](q: Parser[U]): Parser[U] = 
    p~q ^^ { case (x, y) => y }
    
  def <~ [U](q: Parser[U]): Parser[T] = 
    p~q ^^ { case (x, y) => x }
}

println(parse("[a]", char('[') ~> (letter | digitAsInt) <~ char(']'))) // Some((a,))
```
**KRISHNA** Using this, I can start shaping an array parser like this:

```scala
object ParseArray {

  def value = letter | digitAsInt
  
  def array = char('[') ~> value <~ char(']')
  
  def apply(in: String) = parse(in, array)
}

println(ParseArray("[a]")) // Some((a,))
```

**KRISHNA** So this is how we can parse singleton arrays in a functional programming paradigm.  Brahma, can you show me, how will you modify APL code to parse multiple array elements?

**BRAHMA** Sure, lets look at this...

```apl
⍝ TODO
```

**BRAHMA** Krishna, can you evolve your code to show how would you approach parsing multiple array elements?

**KRISHNA** Yes, I'll have to evolve another parser combinator that will take care of parsing many elements and return those as a list of elements.  I'll denote ```many``` parser by symbol ```*```.  

```scala
implicit class ParserExtensions[T](p: Parser[T]) {
  def ~ [U](q: => Parser[U]): Parser[(T,U)] = 
    (in: String) => for {
      (x, in1) <- p(in)
      (y, in2) <- q(in1)
    } yield ((x,y),in2)
  ...
  ...
  import scala.language.postfixOps  
  def * : Parser[List[T]] = 
    p~(p*) ^^ { case (x, xs) => x :: xs } | success(List())
}
```

**KRISHNA** In the many combinator ```*``` implementation, I apply the parser ```p``` once and compose with the same parser recursively until I exhaust all the elements or there are none available.  As ```*``` is applied postfix, I need to add the line ```import scala.language.postfixOps``` else I'll get a warning message from the compiler.

**KRISHNA** Further, I'll have to make sure that for the sequential ```~``` combinator, I use the call-by-name semantics denoted by ```=>``` for the ```q``` parser.  A call-by-name argument ```q``` will be evaluated only when it is needed, which is only after ```p``` has run. If call-by-value semantics is used, it will cause stack overflow without reading any input, when calling the many ```*``` combinator.

**KRISHNA** Finally, I'll have to add the base case for recursion, i.e., for empty string I'd OR ```|``` with ```success(List())``` parser.  

**KRISHNA** Now that I have the ```many``` parser, I can parse many letters

```scala
println(parse("hi", letter*)) // Some((List('h','i'),))
```

**KRISHNA** Now, our array elements are separated by a comma ```,``` delimiter, so I'll create an overloaded version of the many ```*``` parser that takes in a separator parser as a parameter.  Also, just as in the earlier case, I had to take the second parser as call-by-name, so will I do in the ```~>``` combinator.  For symmetry, I'll also do it in ```<~``` combinator.

```scala
implicit class ParserExtensions[T](p: Parser[T]) {
  def ~> [U](q: => Parser[U]): Parser[U] = 
    p~q ^^ { case (x, y) => y }
    
  def <~ [U](q: => Parser[U]): Parser[T] = 
    p~q ^^ { case (x, y) => x }
  ...
  ...
  def * (sep: Parser[Any]): Parser[List[T]] = 
    p~(sep~>p*) ^^ { case (x, xs) => x :: xs } | success(List())
}

println(parse("[a,1]", char('[') ~> (alphanum*(char(','))) <~ char(']'))) // Some((List(a,1),)
```

**KRISHNA** In here, I used the right ```~>``` combinator and drop the separator from the output and the rest is same as the ```*``` many combinator.  So, now I can modify the array parser as:

```scala
object ParseArray {

  def value: Parser[Any] = letter | digitAsInt
  
  def array: Parser[List[Any]] = char('[') ~> (value*(char(','))) <~ char(']')
  
  def apply(in: String) = parse(in, array)
}

println(ParseArray("[a,1]")) // Some((List(a,1),)
```

**BRAHMA** Let me now show you, how can I extend the existing implementation to make nested arrays parsing work.

```apl
⍝ TODO
```

**KRISHNA** For me to implement that change, all I've to do is add the ```array``` clause to the existing grammar and make it recursive.  

```scala
object ParseArray {

  def value: Parser[Any] = array | letter | digitAsInt
  
  def array: Parser[List[Any]] = char('[') ~> (value*(char(','))) <~ char(']')
  
  def apply(in: String) = parse(in, array)
}

println(ParseArray("[a,1,[२,ब]]")) // Some((List(a,1,List(२,ब)),)
```

**BRAHMA** Lets reflect on this...

Reflections
-----------

**KRISHNA** 

**KRISHNA** 

**BRAHMA** 

**BRAHMA** 
