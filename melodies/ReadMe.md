# Melodies

* Control Flow and Data Flow Melody
    * FizzBuzz
    * Luhn   

* Transparency and Abstraction Melody
    * MarsRover 
  
* Macro and Micro Melody
    * Parsing Recursive Structures like Arrays, HTML documents etc...

* ??? Melody
    * Data Reconciliation


## Overall Reflections

**BRAHMA** So from the melodies that we demonstrated, there are a couple of observations that I think are worth jotting down.  The Data-Driven and the Control Flow melody shows us that being data-driven causes the program knowledge to be moved into data and away from the control-flow.  As humans are good at recognizing data visually, than reasoning about logic, being data-driven is important.  

**KRISHNA** Yes and at the same time, one has to work hard to be data-driven in an imperative/object-oriented paradigm, whereas it with array-oriented paradigm being data-driven is the starting point.  In fact, trying to be object-oriented or imperative would be difficult with array-oriented paradigm.  This is strikingly similar to the mutability and immutability in imperative/OO and FP paradigm.  One has to work hard to stay immutable in imperative paradigm, whereas its where FP paradigm begins.  In the FP paradigm, one has to work hard to achieve mutability.

**BRAHMA** Also, another observation is that - AO paradigm helps to discover the underlying maths.  This is because here the only data-structure available are Arrays and its specialised forms like Vector and Scalar - a single dimension value.  This has two effects - frees the developer from this aspect of discovery of the data-structure.  Once free from this, the next natural focus is to come up with a solution to fit the data-structure.  In this process, all array manipulation operations like inner/outer product, transposing, reduction etc… will come to the fore.  This is one that bends the mind in a different direction.  It helps evolve the algorithm to solve the problem - the math way.  So, in other words, it forces you to think about the problem mathematically using numbers and arrays.

**KRISHNA** Yes, APL makes this assumption that all problems are mathematical in nature and can be solved using math.  In general, when faced with a problem, APLers tend to discover the math underlying the problem and make that as the starting point to arrive at a solution.

**BRAHMA**  So, this causes the logic to be flushed out, keeping the data-structure constant.  In other paradigms, you are working at these two things simultaneously.  Once you finish working on that, the next thing is to bring in performance optimisations.  Hence, the order becomes - “make it work, make it better and then make it performant”.   

**KRISHNA**  While being good at this, the we also saw that for problems that are non-mathematical in nature, like parsing or a game where many edge cases are present, then it is better to stay in imperative or OO paradigm, because in those paradigms one can slip back to the control-flow.  It can become difficult to write good APL code in presence of many branches.

**KRISHNA** So each paradigm has its sweet spot.  What I’ve seen is that when faced with tough problems, people use APL, work out the solution there first and re-write the solution in the paradigms which they use in day-to-day work. It is important to know different paradigms, because each one has a different influence on the mind and multi-paradigm approach helps to uncover the solution in novel ways.
