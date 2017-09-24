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
      DPos = maps:get(Key, ?COMPASS),
      {NewX,NewY,NewDeg} = execute({X,Y,Deg},DPos,Cmd),
      {NewD, _} = lists:keyfind(NewDeg rem 36, 2, Keys),
      {NewX,NewY,NewD}
  end.
```

**BRAHMA**  So, here everything is functions, we did not require ```Direction``` or ```Vector``` abstractions, the essence has become more transparent and is there for anyone to see.  Earlier, this was encapsulated away within ```Direction``` and we were using it.  So Krishna, how does this look in an Array-Oriented Paradigm like APL?

**KRISHNA** In an array language, an important goal is to find data representations which allows operations to be applied to large subsets of data - ideally the entire problem set at once. We are moving around in a grid where row and column co-ordinates increase when you move down (South) and to the right (East). Thus, the movement resulting from a move of length 1 in each of these four directions NESW gives us changes to the row and column co-ordinates, which we will store in an array called movement:

        directions←'NESW'
        movement←4 2⍴¯1 0 0 1 1 0 0 ¯1 ⍝ 4x2 matrix

The initial conditions can be stored in 3 arrays as follows:

        position←3 3
        heading←1     ⍝ index into directions
        commands←'MMRMMLMRML'

If we think of the four directions as an array, and represent the direction as an index into this array, then a turn to the left decreases the index by one, while a turn to the right increases the index. The ```index of``` primitive ⍳ allows us to look the commands up in the character array 'LMR' and subtract 1 from the result, we get ¯1 for L, 0 for M and 1 for R. This allows us to compute the direction that the robot is pointing at each step as follows:

        bearings←4|+\heading,'LMR'⍳commands

The 4| gives us the rsult modulus 4, ensuring that we wrap around nicely from W to N or vice versa and that ```bearings``` is an arrays of integers between 0 and 3. If we now index the ```movement``` array by the bearings and mask this out by *multiplying* each row by 1 where the command is M and 0 elsewhere, we get a list of movements made by the robot.

        movements←movement[¯1↓bearings;] (×⍤1 0) 'M'=commands

The rank operator is used above, in (×⍤1 0), to multiply each vector on the left (rank 1 cells) by each scalar on the right (rank 0) cells. If the above gives an INDEX ERROR when you try to reproduce it, remember to set index origin to zero (⎕IO←0). Finally, we can compute the final position by doing a plus reduction on the starting position followed by the list of movements:

        +⌿position⍪movements
    4 4

The final direction is simply the last item of the bearing vector:

        directions[¯1↑bearings]
    E

If we were to collect the above into a function in APL, it might look like this:

     rove←{(position heading commands)←⍵          
           directions←'NESW'
           movement←4 2⍴¯1 0,0 1,1 0,0 ¯1 
           bearings←4|+\heading,'LMR'⍳commands
           movements←movement[¯1↓bearings;]×'M'=commands
           (+⌿position⍪movements),directions[¯1↑bearings] 
          }

Note that there are no conditionals and no loops in the above function: Every operation is applied to dense arrays of small integers. This means that the function will be efficient even when executed by an interpreter, and that a compiler stands a decent chance of running large parts of the code - possibly all of it - in parallel.

**BRAHMA** Let me re-write this in JavaScript again.  So, I don't need ```COMPASS``` macro this time, a simple Array holding the movement tuples would do.  The indices of the array represent directions ```1``` for ```N```, ```2``` for ```E```, ```3``` for ```S``` and ```4``` for ```W```.

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

**BRAHMA** While the FP paradigm embraces transparency of the data-structure but makes the structure immutable, so one need not worry about an inadvertent change to the values.  Again, here as in OO, one has to arrive at the correct data-structure suitable for the problem at hand.

**BRAHMA** On the other hand, in AO paradigm, like APL or J, the only data-structure available are Arrays and its specialised forms like Vector and Scalar - a single dimension value.  This has two effects - frees the developer from this aspect of discovery of the data-structure.  Once free from this, the next natural focus is to come up with a solution to fit the data-structure.  Due to this, all array manipulation operations like inner/outer product, transposing, reduction row-wise, col-wise etc… will come to the fore.  It is this force that bends the mind in a different direction.  

**KRISHNA** Yes, it helps evolve the algorithm to solve the problem - the math way.  So, in other words, it forces you to think about the problem mathematically using numbers and arrays.

**KRISHNA** APL makes this assumption that all problems are mathematical in nature and can be solved using math.  In general, when faced with a problem, APLers tend to discover the math underlying the problem and make that as the starting point to arrive at a solution.

**KRISHNA** In APL, abstraction is considered harmful. This isn't to say that we don't work with abstractions, or at an abstraction, but the process of programming by which we build progressively upon previous abstractions with new abstractions, eventually resulting in some generic framework that we can then plug in our desired instance and obtain a result is considered to be a harmful practice.

**KRISHNA** Also, nature of abstraction is to hide the details, rather than subordinating them. "Good" abstraction hides it so well that you cannot access it. Good programming languages often provide means of solid abstractions that prevent introspection beyond the abstraction boundaries, while languages are often criticized for permitting too much leakiness in their abstraction boundaries.

**KRISHNA** APL on the other hand, embraces transparency of expression, and tends to eschew the ever-increasing introduction of abstraction to solve complexity. Instead, transparency of ideas, and directness of expression serve as the weapons with which the APLer falls upon the enemy called "Complexity" and deals a mortal blow.

**BRAHMA** I see, but what about readability? With the use of non-standard characters, doesn't the code become - "write only"?  How can one maintain such a code?

**KRISHNA** Common programming practice suggests that those unfamiliar with the code base should still get the general idea of the code, which is the idea of "readability." The result is that usually code is deemed to be more readable when it is more verbose, as the expected reader is assumed to now have an intimate knowledge of the entire code base.

**KRISHNA** Indeed, it is presumed that even programmers that spend their whole lives inside of the code base will not be able to keep the whole of the source in their minds at once because of the complexity and size of most non-trivial programs. This leads to a general favoritism to verbose code over concise code, which is considered to be more self-documented even for the experienced programmer.

**KRISHNA** Traditional programming problem solving practice encourage breaking down a problem into very small components, and most programming languages are designed around the idea of providing a clear picture of a small piece of code in isolation of the rest of the code. APL instead emphasizes finding ways of expressing solutions that emphasize the macro-view of the problem, and attempts to eliminate as much as possible the "small details" or the decomposition of a problem into parts that are viewed separately or independently of the whole.

**KRISHNA** APL programmers, on the other hand, often optimize their code so that it is more easily manipulated, and they are more easily able to absorb the code base on the whole. Contrary to traditional wisdom, they often expect that they will be able to see and process significant programs at one time (macro vs. micro) which in turn leads them to favor concision, which allows them to see more of the code, rather than verbosity, which hides more code at a time. Concise code is taken to be more readable both at the small level (because it is easier to play with and explore and understand as a whole) but also at the level of integration, where the more concise code is taken to be easier to integrate into a larger code base and not lose sight of the entire source code.

**BRAHMA** So, essentially this another eye-opening contrast - Transparency or Abstraction? and Concision or Verbosity?  Lets move to the next melody in our jugalbandi.
