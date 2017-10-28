#Parsing Text Containing Recursive Structures

Let us consider the problem of parsing arrays.  Examples could be:

* An empty array - ```"[]"```, 
* An array with single letter - ```"[a]"``` or a single digit - ```"[1]"```,
* An heterogeneous array with a letter and a digit - ```"[a,1]"```,
* Nested Arrays - ```"[[]]", "[a,[]]", "[a,[1]]", "[अ,१,[२,ब]]"```

The BNF (Backus-Naur Form) or EBNF (Extended BNF) grammar for Arrays containing letters, digits and nested arrays can be described as:

```
value ::= array | letter | digit.
array ::= "[" [ values ] "]".
values ::= value {"," value}.
```

Notation      | Meaning
------------- | -------------
```|```       | Alternative
```[ ... ]``` | Optional
```{ ... }``` | Repeated Zero, or One or Many times

 
**_NOTE_**: For simplicity, we will not consider whitespaces to be parsed.
 

Code Jugalbandi in Functional Programming & Array-Oriented Paradigms
----

**BRAHMA** We have agreed to play three verses: first we will write a parser for an array containing a single digit or letter (or none). Then we will look at how we need to adapt our solutions in order to extend them, first to parse arrays with more than one elements, and finally nested arrays, where each item of an array can be another array.

**BRAHMA** Lets play the first verse in APL. The terseness of APL and the ability to experiment interactively encourages an "extreme" approach to solving problems, so I choose to write a minimal solution to the zero-or-one element case:

```apl
    ArrayParser←{
       '[]'≢(⊣/,⊢/)⍵ : 'missing outer []' ⎕SIGNAL 11 
       1<≢inner←1↓¯1↓⍵ : 'max one item allowed' ⎕SIGNAL 11
       ⊃inner∊⎕D: ⎕D⍳inner
       inner
     }
```

**BRAHMA** Each of the first three lines is a ```guard```, containing two expressions separated by a colon.  The expression on the left is a logical test; if the test is true the second expression is executed and it's result becomes the result of the function. In this case, the first two lines are performing validation.

**BRAHMA** The first test ```'[]'≢(⊣/,⊢/)⍵``` determines whether the string "[]" is different from a string formed from the first and last element of the argument.  If this is the case, we throw an exception using ```⎕SIGNAL``` and the function terminates.  We use the standard APL event number 11 (DOMAIN ERROR) with a customer error message, provided as the left argument.

**BRAHMA** The second test expression forms a new string called ```inner``` by dropping the first and last character from the argument.  If the length of inner is greater than one, an exception is thrown.

**BRAHMA** The third guard expression tests whether the first (and only) element of inner is a member of ```⎕D```, a system constant containing the digits 0..9.  If it is, the expresion on the right returns the index of the character in this array, essentially casting the character to an integer.  If the character is not a digit, we fall through to the 4th line, which simply returns the string ```inner``` as the result.

**BRAHMA** Krishna, how would you play this tune in an OO or Functional paradigm?

**KRISHNA** Ok, let me show you how this can be approached in a functional programming paradigm.  I'd like to take full advantage of functional programming and create a solution which may require a bit more work to begin with, but will be easy to extend to the more complex cases and actually allow us to more or less use BNF 

**KRISHNA** I'll use Scala once again, to construct a framework of ```parsers``` and ```infix``` compositional forms that will allow me to write statements which closely resemble BNF and have them produce an executable parser.  We can define a parser as a function that consumes a ```String``` and produces a structure of type ```T```:

```scala
type Parser[T] = String => T
```

**KRISHNA**  A useful technique is to define a number of parsers which each look for a specific pattern and - if they find it - "consume" the part of the input string corresponding to that pattern.  We can change the return type to a tuple and also return the unused part of the string:

```scala
type Parser[T] = String => (T, String)
```

**KRISHNA** Each parser may or may not find the pattern it is looking for.  We can represent this by making the return t of the type ```T```optional:

```scala
type Parser[T] = String => Option[(T, String)]
```

**KRISHNA** Having defined parser type, we can now start implementing parsers.  A functional language allows us to start with simple parsers to find single digits or letters, and then use combinators to produce compound parsers which search for sequences of patterns (a sequence of digits gives a number) - or look for alternative patterns (an array item can be number or a string).

**KRISHNA** The first parser we will implement is one that consumes a single character from the string and returns that character along with the unused string:

```scala
def item = (in: String) => 
  if (in.isEmpty) None else Some((in.head, in.tail))

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

**KRISHNA** Now, that we have these primitive parsers, we can start building complex parsers  by combining them.  Taking cues from BNF grammar - a language for defining a language, wherein it composes bigger grammar from smaller grammer. BNF/EBNF grammar for an array can be given as:

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

**KRISHNA** I'll make `seq` sequential composition an infix operator, so that its possible to translate BNF grammar to a parser specification without much effort.  For this I'll resort to Scala's ```implicit class``` mechanism and move the ```seq``` method inside.   Scala requires that an implicit class must have a constructor that takes in a parameter on which the operator can be invoked (in other words a receiver).  So the first parameter ```p: Parser[T]``` goes in as constructor parameter and the second remains with the method itself.  I'll also rename ```seq``` to symbol ```~```, here is how it looks:

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

**KRISHNA** Many a times, we simply don't want to have the nested tuples in the output like the above, its messy to extract values out of it. So, we would rather not return the output of the first parser, instead simply chain that output as input to the second parser in sequence.  I'll then return the output of the second parser - which comprises of its actual output value and residual input after both the parsers have done their job.  To enable chaining, in place of the second parameter ```q: Parser[U]```, i'll install a function that consumes the output produced by the first parser and returns the second parser.  We define another operator called ```chain``` or ```flatMap``` as generally named in Scala.   Haskell calls it ```bind``` or ```shove``` for it.  For this, i'll use the Haskell symbol ```>>=``` and define it within ParserExtensions so that its available in its infix form.  

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

**KRISHNA** Usually we are interested in parsing particular characters. I should be able to specify which characters I'm interested in parsing and rejecting if it does not match my specification.  Using the above definition, I can now define another simple and a useful parser that consumes a character from input and checks that against a given predicate.  If the predicate is satisfied, then it returns that character else it simply fails.  Lets call this character parser ```satisfy```.  

```scala
def satisfy(pred: Char => Boolean): Parser[Char] = ???
```

**KRISHNA** If we recall, the earlier ```item``` parser consumed a character without any reservations, so we will use that to chain its output and apply the predicate.  If the predicate fails, return a failure parser and if it succeeds, return the accepted character in a success parser along with the residual input.

```scala
def satisfy(pred: Char => Boolean): Parser[Char] = 
  item >>= (ch => if (pred(ch)) success(ch) else failure)
    
println(parse("hello", satisfy(_ == 'h'))) // Some((h,ello))
println(parse("world", satisfy(_ == 'h'))) // None
println(parse("", satisfy(_ == 'h')))      // None
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

**KRISHNA** But I don't have the choice combinator, so I'll create one.  Now again, in effort to go closer to  BNF symbols used for defining grammar, I'll use the ```|``` symbol.  The choice combinator applies the first parser, if ```p``` fails, it tries the second parser ```q``` on the same input (allows back-tracking) and returns the output of the second parser.  In case the first succeeds, it returns the output from the first and does not evaluate the second.

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

**KRISHNA**  Also look at the BNF/EBNF grammar and see how the above closely resembles its spec.

```
value ::= array | letter | digit.
array ::= "[" [ values ] "]".
values ::= value {"," value}.
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
  ...
  ...
  def ~> [U](q: => Parser[U]): Parser[U] = 
    p~q ^^ { case (x, y) => y }
    
  def <~ [U](q: => Parser[U]): Parser[T] = 
    p~q ^^ { case (x, y) => x }

  def * (sep: Parser[Any]): Parser[List[T]] = 
    p~(sep~>p*) ^^ { case (x, xs) => x :: xs } | success(List())
}
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
**KRISHNA** So this is how we can parse multiple array elements with a separator/delimiter.  How will you make nested parsing work in APL?

**BRAHMA** Let me now show you, how can I extend the existing implementation to make nested arrays parsing work.

```apl
⍝ TODO
```

**BRAHMA** How would you do this in Scala?

**KRISHNA** To implement that change, all I've to do is add the ```array``` clause to the existing grammar and make it recursive.  

```scala
object ParseArray {

  def value: Parser[Any] = array | letter | digitAsInt
  
  def array: Parser[List[Any]] = char('[') ~> (value*(char(','))) <~ char(']')
  
  def apply(in: String) = parse(in, array)
}

println(ParseArray("[a,1,[ब,२]]")) // Some((List(a,1,List(ब,२)),)
```

**BRAHMA** Lets reflect on this...

Reflections
-----------
<!--TODO-->

**KRISHNA** 

**KRISHNA** 

**BRAHMA** 

**BRAHMA** 
