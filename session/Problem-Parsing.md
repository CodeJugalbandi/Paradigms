# Parsing Text Containing Recursive Structures

Let us consider the problem of parsing arrays.  Examples could be:

* An empty array - ```"[]"```, 
* An array with single letter - ```"[a]"``` or a single digit - ```"[1]"```,
* An heterogeneous array with a letter and a digit - ```"[a,1]"```,
* Nested Arrays - ```"[[]]", "[a,[]]", "[a,[1]]", "[अ,१,[२,ब]]"```

The BNF (Backus-Naur Form) or EBNF (Extended BNF) grammar for Arrays containing letters, digits and nested arrays can be described as:

```
array  ::= "[" [ values ] "]".
values ::= value {"," value}.
value  ::= array | letter | digit.
```

Notation      | Meaning
------------- | -------------
```|```       | Alternative
```[ ... ]``` | Optional
```{ ... }``` | Repeated Zero, or One or Many times

 
**_NOTE_**: For simplicity, we will not consider whitespaces to be parsed.
 
