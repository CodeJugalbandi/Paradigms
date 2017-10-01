#Mars Rover

A robotic rover is to to be landed by NASA on a plateau on Mars. This plateau, which is curiously rectangular, must be navigated by the rover.

A rover's position and location is represented by a combination of x and y co-ordinates and a letter representing one of the four cardinal compass points. The plateau is divided up into a grid to simplify navigation. An example position might be 0, 0, N, which means the rover is in the bottom left corner and facing North.

In order to control a rover, NASA sends a simple string of letters. The possible letters are 'L', 'R' and 'M'. 'L' and 'R' makes the rover spin 90 degrees left or right respectively, without moving from its current spot.  'M' means move forward one grid point, and maintain the same heading. 

Assume that the square directly North from (x, y) is (x, y+1).

###INPUT
Rover has two lines of input. The first line gives the rover's position, and the second line is a series of instructions telling the rover how to explore the plateau.  The position is made up of two integers and a letter separated by spaces, corresponding to the x and y co-ordinates and the rover's orientation.

###OUTPUT 
The output for each rover should be its final co-ordinates and heading.

###Test Input and Output

####INPUT
```shell
3 3 E
MMRMMLMRML
```
####OUTPUT
```shell
6 0 E
```

Code Jugalbandi in Object-Oriented, Functional Programming & Array-Oriented Paradigms
----

**BRAHMA**  Lets first look at Object-Oriented Paradigm and I'll use JavaScript's OO facilities to solve this problem.  So we need a ```MarsRover``` that can take in the starting position of the rover with a behavior of ```rove``` that takes in a ```command``` sent from Earth. 

```javascript
class MarsRover {
  constructor(x, y, dirString) {}
  rove(command) {}
  toString() {}
}

var rover = new MarsRover(3, 3, 'E');
console.info(rover.rove('M').rove('R').rove('M').rove('L').toString());
```

**BRAHMA**  Now, I'll need to implement behavior of ```rove``` for each of the ```command```s ```L```, ```R``` and ```M```.  Starting with ```M```, say, I'm facing North, I'll increment ```y``` and not ```x```, if I'm facing West, I'll decrement ```x``` and not ```y``` and for the remaining directions, which are duals, the operations will be modifying either ```x``` or ```y```.  Lets take ```L```, for turning left is also relative to direction and so is turning right at a position.  Therefore, I need an abstraction which is direction and position aware.  Simply being either direction or position aware will not help.  From physics, ```Vector``` as an notion seems to fulfill this, it has position as a scalar and direction along with it.  So, for each command let us tell the ```Vector``` to do the job.  For ```M```, we tell the ```Vector``` to move forward, for ```L```, we tell the ```Vector``` to turn left and for ```R``` we tell the ```Vector``` to turn right.  We will also maintain the state of the rover as a ```Vector``` itself.


```javascript
class Vector {
  constructor(x, y, dirString) {
    this.x = x;
    this.y = y;
    this.dirString = dirString;
  }
  moveForward() { return this; }
  turnLeft() { return this; }
  turnRight() { return this; }
  toString() { return `${this.x} ${this.y} ${this.dirString}`;}
}

class MarsRover {
  constructor(x, y, dirString) {
    this.vector = new Vector(x, y, dirString);
  }
    
  rove(command) {
    if(command === 'M') 
      this.vector = this.vector.moveForward();
    else if(command === 'L') 
      this.vector = this.vector.turnLeft();
    else if(command === 'R') 
      this.vector = this.vector.turnRight();

    return this;
  }
    
  toString() {
    return this.vector.toString();
  }
}

var rover = new MarsRover(3, 3, 'E');
console.info(rover.rove('M').rove('R').rove('M').rove('L').toString());

```

**BRAHMA**  Now, I need to implement the 3 behaviors in ```Vector```.  Lets say we want to ```turnLeft```, its turning left from the current direction, keeping position (```x``` and ```y```) same. Say we want to ```turnRight```, its turning right from the current direction, keeping position same again and if its ```moveForward```, its moving forward a position in current direction based on current position, keeping direction same.  As everything is direction based, I now need ```Direction``` as an abstraction.  I'll code ```Direction``` the old ```function``` based as this new ```class``` is bit too verbose!

```javascript
function Direction(dirString) {
  this.moveForward = (x, y) => [x, y];
  this.left = () => this;
  this.right = () => this;
  this.toString = () => dirString;
}

class Vector {
  constructor(x, y, dirString) {
    this.x = x;
    this.y = y;
    this.direction = new Direction(dirString);
  }
  moveForward() { 
    var [newX, newY] = this.direction.moveForward(this.x, this.y);
    return new Vector(newX, newY, this.direction.toString()); 
  }
  turnLeft() { return new Vector(this.x, this.y, this.direction.left().toString()); }
  turnRight() { return new Vector(this.x, this.y, this.direction.right().toString()); }
  toString() { return `${this.x} ${this.y} ${this.direction.toString()}`;}
}
```

**BRAHMA**  Now, I need to implement the 3 behaviors in ```Direction```.  Say current direction is ```N```, so West is to the ```left``` of North and East is to the ```right``` of North.  So, it is for other directions - all fixed.  So, I can hard-code these directions.  To move forward in North, I have to increment ```y``` by ```1```, keeping ```x``` same.

To achieve all this, I need a data-structure to hold all the four directions and the respective transformations.  I'll use a ```Map``` with current direction as a key - lets call it ```allDirections```.  Values corresponding to each transformation will be held in an ```Array```.  So, it will look like this:

```javascript
function Direction(dirString) {
  let directions = {
    'N' : ['W', 'E', (x,y) => [x, y+1]],
    'E' : ['N', 'S', (x,y) => [x+1, y]],
    'S' : ['E', 'W', (x,y) => [x, y-1]],
    'W' : ['S', 'N', (x,y) => [x-1, y]]
  };
  
  this.moveForward = (x, y) => directions[dirString][2](x, y);
  this.left = () => new Direction(directions[dirString][0]);
  this.right = () => new Direction(directions[dirString][1]);
  this.toString = () => dirString;
}
```

**BRAHMA**  So, finally, we have the output.

```javascript
function Direction(dirString) {
  var allDirections = {
    'N' : ['W', 'E', (x,y) => [x, y+1]],
    'E' : ['N', 'S', (x,y) => [x+1, y]],
    'S' : ['E', 'W', (x,y) => [x, y-1]],
    'W' : ['S', 'N', (x,y) => [x-1, y]]
  };
  
  this.moveForward = (x, y) => allDirections[dirString][2](x, y);
  this.left = () => new Direction(allDirections[dirString][0]);
  this.right = () => new Direction(allDirections[dirString][1]);
  this.toString = () => dirString;
}

class Vector {
  constructor(x, y, dirString) {
    this.x = x;
    this.y = y;
    this.direction = new Direction(dirString);
  }
  moveForward() { 
    var [newX, newY] = this.direction.moveForward(this.x, this.y);
    return new Vector(newX, newY, this.direction.toString()); 
  }
  turnLeft() { 
    return new Vector(this.x, this.y, this.direction.left().toString()); 
  }
  turnRight() { 
    return new Vector(this.x, this.y, this.direction.right().toString()); 
  }
  toString() { 
    return `${this.x} ${this.y} ${this.direction.toString()}`;
  }
}

class MarsRover {
  constructor(x, y, dirString) {
    this.vector = new Vector(x, y, dirString);
  }
    
  rove(command) {
    if(command === 'M') 
      this.vector = this.vector.moveForward();
    else if(command === 'L') 
      this.vector = this.vector.turnLeft();
    else if(command === 'R') 
      this.vector = this.vector.turnRight();

    return this;
  }
    
  toString() {
    return this.vector.toString();
  }
}

var rover = new MarsRover(3, 3, 'E');
console.info(rover.rove('M').rove('R').rove('M').rove('L').toString());

```

**KRISHNA**  This is highly readable, but has duplication and pretty verbose. 

**BRAHMA**  Yes indeed, I see that too! Let me show you how we can reduce this verbosity in Erlang, using a purely Functional Programming Paradigm.  So i'll begin by creating a module and export the ```rove``` function that takes in current position and a command.  This function will use a ```case``` on ```Cmd``` to do the needful.

```erlang
-module (marsrover).
-export ([rove/2]).

rove(Vec, Cmd) ->
  case Cmd of
    'M' -> Vec;
    'L' -> Vec;
    'R' -> Vec;
    _ -> Vec
  end.
```

**BRAHMA** I'll now remove the duplication present in the earlier ```Map``` that we evolved when coding in JavaScript.  Here I'll define a macro called ```COMPASS``` which holds directions along with the relevant degrees that one finds on a regular compass as a tuple.  This tuple will be the key and the corresponding value will be the movement transformation in that direction.  In fact, this duplication can also be refactored away to optimize the earlier JavaScript code.

```erlang
-module (marsrover).
-export ([rove/2]).
-define (COMPASS, #{
    {'N',0} =>  {0,1},
    {'E',9} =>  {1,0},
    {'S',18} => {0,-1},
    {'W',27} => {-1,0}
}).

rove(Vec={X,Y,Dir},Cmd) ->
  Keys = maps:keys(?COMPASS),
  case lists:keyfind(Dir, 1, Keys) of
    not_found ->
      Vec;
      
    Key={_,Deg} ->
      {Dx,Dy} = maps:get(Key, ?COMPASS),
      {NewX,NewY,NewDeg} = case Cmd of
        'M' -> 
          {X+Dx,Y+Dy,Deg};
        'L' -> 
          {X,Y,Deg-9};
        'R' -> 
          {X,Y,Deg+9};
        _ -> Vec
      end,
      {NewD, _} = lists:keyfind(NewDeg rem 36, 2, Keys),
      {NewX,NewY,NewD}
  end.
```

**BRAHMA** I'll just refactor the above code to shove the ```Cmd``` case inside the ```execute``` function.

```erlang
-export ([rove/2]).
-define (COMPASS, #{
    {'N',0} =>  {0,1},
    {'E',9} =>  {1,0},
    {'S',18} => {0,-1},
    {'W',27} => {-1,0}
}).

execute(Vec={X,Y,Deg}, {Dx,Dy}, Cmd) ->
  case Cmd of
    'M' -> 
      {X+Dx,Y+Dy,Deg};
    'L' -> 
      {X,Y,Deg-9};
    'R' -> 
      {X,Y,Deg+9};
    _ -> Vec
  end.
  
rove(Vec={X,Y,Dir},Cmd) ->
  Keys = maps:keys(?COMPASS),
  case lists:keyfind(Dir, 1, Keys) of
    not_found ->
      Vec;
      
    Key={_,Deg} ->
      Delta = maps:get(Key, ?COMPASS),
      {NewX,NewY,NewDeg} = execute({X,Y,Deg},Delta,Cmd),
      {NewD, _} = lists:keyfind(NewDeg rem 36, 2, Keys),
      {NewX,NewY,NewD}
  end.
```

**BRAHMA**  So, here everything is functions, we did not require ```Direction``` or ```Vector``` abstractions, the essence has become more transparent and is there for anyone to see.  Earlier, this was encapsulated away within ```Direction``` and we were using it.  So Krishna, how does this look in an Array-Oriented Paradigm like APL?


**KRISHNA** In an array language, an important goal is to find data representations which allows operations to be applied to large subsets of data - ideally the entire problem set at once. If positions are represented by a pair of numbers giving the longditude (E=positive) and latitude (N=positive), then a movement in any one of the the four directions NESW represents a movement of (0 1),(1 0),(0 ¯1) and (¯1 0), which we choose to represent as a 4-row matrix:

```apl
  ⎕IO←0    ⍝ Default index origin in APL is 1; we prefer 0
  directions←'NESW'
  movement←4 2⍴0 1,1 0,0 ¯1,¯1 0 ⍝ One row per direction (NESW)   
```

**KRISHNA** The initial conditions can be stored in 3 arrays as follows:

```apl
  position←3 3  ⍝ intial position
  heading←1     ⍝ index into directions
  commands←'MMRMMLMRML'
```

**KRISHNA** The four directions and corresponding movements are arrays. We can represent the current direction as an index into these arrays. Since we ordered them in clockwise order, our three possible commands L, M and R will result in a change in the direction index of ¯1, 0 and 1, respectively. This allows us to simply compute the direction at the end of each move by doing a sum scan of the starting position and a vector in which the commands have been mapped to the directional change that they produce:

```apl
  bearings←4|+\heading,¯1+'LMR'⍳commands
```

**KRISHNA** The ```⍳```function produces the indices into 'LMR' of our command stream, from which we subtract 1. The ```4|``` gives us the result modulus 4, ensuring that we wrap around nicely from W to N or vice versa and that ```bearings``` is now an array of integers between 0 and 3. If we now index the ```movement``` array by the bearings and mask this out by *multiplying* each row by 1 where the command is M and 0 elsewhere, we get a list of movements made by the robot.

```apl
  movements←movement[¯1↓bearings;] (×⍤1 0) 'M'=commands
```
**KRISHNA** We use multiplication with left rank 1 and right rank 9 (```(×⍤1 0)```), to multiply each row (pair of movements) on the left by each element of the Boolean vector on the right, which contains 1 for each M and 0 for all other commands. Finally, we can compute the final position by doing a plus reduction on 
the starting position followed by the list of movements:

```apl
  +⌿ position⍪movements
6 6
```

**KRISHNA** The final direction is given by the last item of the vector of bearings, if we index the ```directions``` array, we can translate it back into a character:

```apl
  directions[¯1↑bearings]
E

```

**KRISHNA** If we were to collect the above into a function in APL, it might look like this:

```apl
   rove←{⎕IO←0 ⋄ directions←'NESW'
         movement←4 2⍴0 1,1 0,0 ¯1,¯1 0       ⍝ One row per direction (NESW)    
         (position heading commands)←⍵        ⍝ Deconstruct right argument
         direction←directions⍳heading          ⍝ Direction index
         bearings←4|+\direction,¯1+'LMR'⍳commands   ⍝ Bearing after each command
         moves←movement[¯1↓bearings;](×⍤1 0)'M'=commands ⍝ Movement resulting from each command
         (+⌿position⍪moves),directions[¯1↑bearings] ⍝ Return final position and bearing
        }

```

**KRISHNA** We could call it as follows:
```apl
  rove (3 3) 'E' 'MMRMMLMRML'
6 0 E
```

**KRISHNA** Note that there are no conditionals and no loops in the above function. the operations are applied to dense arrays of small integers or chacters. The algorithm steps which would be "switches" in most languiages are implemented by translating command letters LMR into numbers representing the rotation (¯1 0 1), by multiplying potential movements by the result of comparing the commands to M, and by indexing into a movement array using the direction at the end of each command. This means that the function will be efficient even when executed by an interpreter, and also that it can be compiled for highly data parallel execution if suitable hardware is available.

**BRAHMA** Let me re-write this in JavaScript again.  So, I don't need ```Vector``` or ```Direction``` abstractions this time, a simple Array holding the movement tuples would do.  The indices of the array represent directions ```1``` for ```N```, ```2``` for ```E```, ```3``` for ```S``` and ```4``` for ```W```.

```javascript
function MarsRover(x, y, dirString) {
  let directions = ['N', 'E', 'S', 'W'];
  let directionIdx = Math.max(0, directions.indexOf(dirString));

  let point = [x, y];
  let movements = [[0,1], [1,0], [0,-1], [-1,0]];
  let toIndex = directionValue => Math.abs(directionValue % 4)
  
  this.rove = function(command) {
    if(command === 'M')
      point = point.map((value, idx) => value + movements[directionIdx][idx]);
    else if (command === 'L') 
      directionIdx--;
    else if(command === 'R') 
      directionIdx++;
    
    return this;
  }
  
  this.toString = () => `${point[0]} ${point[1]} ${directions[toIndex(directionIdx)]}`
}

var rover = new MarsRover(3, 3, 'E');
console.info(rover.rove('M').rove('R').rove('M').rove('L').toString());
```

**BRAHMA**  So, thats it.  Lets reflect on what we did...


Reflections
-----------

**BRAHMA** In OO paradigm, we arrive at a suitable data-structure for the problem by the act of continuous refactoring.  If one pays attention to that aspect, then it leads to optimisation of the data-structure for a particular behaviour or a set of behaviours for an object.

**BRAHMA** The FP paradigm embraces transparency of the data-structure but makes the structure immutable, so one need not worry about an inadvertent change to the values.  Again, here as in OO, one has to arrive at the correct data-structure suitable for the problem at hand.

**BRAHMA** In the AO paradigm, like APL or J, the only data-structures available are Arrays (Vectors and Scalars are also arrays with one or zero dimensions).  The exact choice of data structure will affect the elegance and performance of the solution, but the powerful collection of mathematical array manipulation operations like inner/outer product, transposing, reductions and scans along selected dimensions can be easily applied to arrays of any rank, shape or depth.  It is this force that bends the mind in a different direction.  

**KRISHNA** Yes, it helps evolve the algorithm to solve the problem.  AO encourages you to think about the problem mathematically using numbers and arrays.  Having array operations available interactively allows APL to act as a tool of thought, helping the programmer "mine" for algorithms, as well as a deeper understanding of the data, through experimentation.

**KRISHNA** As a result, when faced with a problem, APLers are more likely to search for the math underlying the problem and use that as the starting point. This often leads to significantly simpler  - and often more generalisable - algorithms.  This does mean that feeling comfortable with basic mathematics is a significant factor for an APL programmer.

**KRISHNA** In APL, abstractions which hide information, or involves multiple layers of generic frameworks with unknown inner workings, are considered potentially dangerous - because they can remove the ability to reason about the overall solution, or to answer questions that were not considered in the original design.

If we consider the Mars Rover example, encapsulating the rover as an object with state (OO) or a function which maps position and a command to a new position (FP), makes it harder to realise that the direction that the robot is pointing in at any given time can be computed using a set of potentially parallelisable steps. The representation of the states of the rover as a set of arrays also makes it easy to mine the data to answer new questions like "how much time did the robot spend pointing in each compass direction?"

**KRISHNA** Also, the nature of abstraction is to hide the details, rather than subordinating them. A "Good" abstraction hides it so well that you cannot access it. Introspection beyond the abstraction boundaries is criticized for being leaky and tightly coupled or outright as breaking encapsulation in OO.

**KRISHNA** APL on the other hand, embraces transparency of expression, and tends to eschew the ever-increasing introduction of abstraction to solve complexity. Instead, transparency of ideas, and directness of expression serve as the weapons with which the APLer falls upon the enemy called "Complexity" and deals a mortal blow.

**BRAHMA** I see, but what about readability? With the use of non-standard characters, doesn't the code become - "write only"?  How can one maintain such a code?

**KRISHNA** Common programming practice suggests that those unfamiliar with the code base should still get the general idea of the code, which is the idea of "readability." The result is that usually code is deemed to be more readable when it is more verbose, as the expected reader is assumed to now have an intimate knowledge of the entire code base.

**KRISHNA** Indeed, it is presumed that even programmers that spend their whole lives inside of the code base will not be able to keep the whole of the source in their minds at once because of the complexity and size of most non-trivial programs. This leads to a general favoritism to verbose code over concise code, which is considered to be more self-documented even for the experienced programmer.

**KRISHNA** Traditional programming problem solving practice encourage breaking down a problem into very small components, and most programming languages are designed around the idea of providing a clear picture of a small piece of code in isolation of the rest of the code. APL instead emphasizes finding ways of expressing solutions that emphasize the macro-view of the problem, and attempts to eliminate as much as possible the "small details" or the decomposition of a problem into parts that are viewed separately or independently of the whole.

**KRISHNA** APL programmers, often optimize their code so that it is more easily manipulated, and they are more easily able to absorb the code base on the whole. Contrary to traditional wisdom, they often expect that they will be able to see and process significant programs at one time (macro vs. micro) which in turn leads them to favor concision, which allows them to see more of the code, rather than verbosity, which hides more code at a time. Concise code is taken to be more readable both at the small level (because it is easier to play with and explore and understand as a whole) but also at the level of integration, where the more concise code is taken to be easier to integrate into a larger code base and not lose sight of the entire source code.

**BRAHMA** So, essentially this another eye-opening contrast - Transparency or Abstraction? and Concision or Verbosity?  Lets move to the next melody in our jugalbandi.
