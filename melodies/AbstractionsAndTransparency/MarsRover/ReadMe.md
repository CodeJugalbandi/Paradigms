#Mars Rover

A robotic rover is to to be landed by NASA on a plateau on Mars. This plateau, which is curiously rectangular, must be navigated by the rover.

A rover's position and location is represented by a combination of x and y co-ordinates and a letter representing one of the four cardinal compass points. The plateau is divided up into a grid to simplify navigation. An example position might be -1, 2, N, which means the rover is one distance unit West, and 2 North of the origin and facing North.

In order to control the rover, NASA sends a simple string of letters. The possible letters are 'L', 'R' and 'M'. 'L' and 'R' makes the rover spin 90 degrees left or right respectively, without moving from its current spot. 'M' means move forward one grid point, and maintain the same heading.

Our task is to write a function which takes as input a starting position and a string containing command letters, and returns the resulting position and orientation of the rover.

For example, for a starting position of (3,3,E) and a command sequence "MMRMMLMRML", the resultr should be (6,0,E).

**BRAHMA**  This feels like a classical case for an Object-Oriented solution: our rover is an instance of an object, it has state which records the current position, and the execution of each command modifies that state.  

**BRAHMA** I will use JavaScript: we need a ```MarsRover``` class with a constructor that records the starting position of the rover, and a behavior of ```rove``` that executes a ```command``` sent from Earth. 

```javascript
function MarsRover(x, y, direction) {
  this.rove = (command) => this;
  this.toString = () => `${x} ${y} ${direction}`
}

var rover = new MarsRover(3, 3, 'E');
console.info(rover.rove('M').rove('R').rove('M').rove('L').toString());
```

**BRAHMA**  Now, I'll need to implement ```rove``` for each of the ```command```s ```L```, ```R``` and ```M```.  Starting with ```M```, say, I'm facing North, I'll increment ```y``` and not ```x```, if I'm facing West, I'll decrement ```x``` and not ```y``` and for the remaining directions, which are duals, the operations will be modifying either ```x``` or ```y```.  Lets take ```L```, for turning left is also relative to direction and so is turning right at a position.  Therefore, I need an abstraction which is direction and position aware and has all the operations like turning left or right on it.  From physics, ```Vector``` as a notion seems to fulfill this, it has position as a scalar and direction along with it.  So, for each command we tell the ```Vector``` to do the job.  For ```M```, we tell the ```Vector``` to move forward, for ```L```, we tell the ```Vector``` to turn left and for ```R``` we tell the ```Vector``` to turn right.  We will also maintain the state of the rover as a ```Vector``` itself.  Also notice that using ```commands``` as a ```Map``` data-structure, I've captured the knowledge as data, rather than using a control-flow ```switch-case``` structure.

```javascript
function MarsRover(x, y, dirString) {
  var vector = new Vector(x, y, dirString);

  var commands = {
    'M': vector => vector.moveForward(),
    'L': vector => vector.turnLeft(),
    'R': vector => vector.turnRight()
  };
  
  this.rove = command => {
    vector = commands[command](vector);
    return this;
  }

  this.toString = () => vector.toString();
}

function Vector(x, y, direction) {
  this.moveForward = () => this;
  this.turnLeft = () => this; 
  this.turnRight = () => this; 
  this.toString = () => `${x} ${y} ${direction}`;
}

var rover = new MarsRover(3, 3, 'E');
console.info(rover.rove('M').rove('R').rove('M').rove('L').toString());

```

**BRAHMA**  Now, I need to implement the 3 behaviors in ```Vector```.  Lets say we want to ```turnLeft```, its turning left from the current direction, keeping position (```x``` and ```y```) same. Say we want to ```turnRight```, its turning right from the current direction, keeping position same again and if its ```moveForward```, its moving forward a position in current direction based on current position, keeping direction same.  

**BRAHMA** As everything is direction based, I'll need a data-structure to hold all the four directions and the respective transformations.  I'll again use a ```Map``` with current direction as a key - lets call it ```directions```.  Values corresponding to each transformation will be held in an ```Array```.  

```javascript
function Vector(x, y, direction) {
  var directions = {
    'N' : ['W', 'E', (x,y) => [x, y+1]],
    'E' : ['N', 'S', (x,y) => [x+1, y]],
    'S' : ['E', 'W', (x,y) => [x, y-1]],
    'W' : ['S', 'N', (x,y) => [x-1, y]]
  };
  
  this.moveForward = () => {
    var [newX, newY] = directions[direction][2](x, y);
    return new Vector(newX, newY, direction); 
  };
  
  this.turnLeft = () => new Vector(x, y, directions[direction][0]); 
  this.turnRight = () => new Vector(x, y, directions[direction][1]); 
  this.toString = () => `${x} ${y} ${direction}`;
}
```

**BRAHMA**  In other words, using ```Vector``` and ```MarsRover``` abstractions,  I've organised data and related behavior together.  So, below is the whole code together:

```javascript
function Vector(x, y, direction) {
  var directions = {
    'N' : ['W', 'E', (x,y) => [x, y+1]],
    'E' : ['N', 'S', (x,y) => [x+1, y]],
    'S' : ['E', 'W', (x,y) => [x, y-1]],
    'W' : ['S', 'N', (x,y) => [x-1, y]]
  };

  this.moveForward = () => {
    var [newX, newY] = directions[direction][2](x, y);
    return new Vector(newX, newY, direction);
  };

  this.turnLeft = () => new Vector(x, y, directions[direction][0]);
  this.turnRight = () => new Vector(x, y, directions[direction][1]);
  this.toString = () => `${x} ${y} ${direction}`;
}

function MarsRover(x, y, dirString) {
  var vector = new Vector(x, y, dirString);

  var commands = {
    'M': vector => vector.moveForward(),
    'L': vector => vector.turnLeft(),
    'R': vector => vector.turnRight()
  };
  
  this.rove = command => {
    vector = commands[command](vector);
    return this;
  }

  this.toString = () => vector.toString();
}

var rover = new MarsRover(3, 3, 'E');
console.info(rover.rove('M').rove('R').rove('M').rove('L').toString());
```

**KRISHNA**  Each of the above lines is highly readable, but there are a lot of lines and a fair amount of duplication! 

**BRAHMA**  Yes indeed, I see that too! Let me show you how we can reduce this verbosity further, using a purely Functional Programming Paradigm.  I'll now remove the duplication present in the earlier ```Map``` and I'll define a variable ```compass``` which holds directions along with the relevant degrees that one finds on a regular compass as a tuple.  This tuple will be the key and the corresponding value will be the movement transformation in that direction.  In ES6, I can define an array of items containing key and value both as tuples.

```javascript
var compass = new Map([
    [['N',00], [0,1]],
    [['E',09], [1,0]],
    [['S',18], [0,-1]],
    [['W',27], [-1,0]]
]);
```

**BRAHMA**  I'll now define ```rove``` function that takes x,y and direction as a tuple along with a command

```javascript
function rove([x,y,d], cmd) {
  var keys = Array.from(compass.keys());
  var [newPos] = keys.filter(([dir,deg]) => dir === d)
    .map(key => {
      var [dx,dy] = compass.get(key);
      var [dir, deg] = key;
      return ???(cmd, [x,y,deg],[dx,dy])
    })
}
```

**BRAHMA** In order to apply the command, I'll define another structure ```commands``` with ```command``` as a key and the corresponding transformation functions as the values.

```javascript
var commands = {
    'L': ([x,y,deg],[dx,dy]) => [x,y,deg-9],
    'R': ([x,y,deg],[dx,dy]) => [x,y,deg+9],
    'M': ([x,y,deg],[dx,dy]) => [x+dx,y+dy,deg]
};
```
**BRAHMA** So now the rove function looks like:

```javascript
function rove([x,y,d], cmd) {
  var keys = Array.from(compass.keys());
  var [newPos] = keys.filter(([dir,deg]) => dir === d)
    .map(key => {
      var [dx,dy] = compass.get(key);
      var [dir, deg] = key;
      return commands[cmd]([x,y,deg],[dx,dy]);
    })
}
```
**BRAHMA** I further ```map``` this output to convert ```deg```rees back to ```dir```ection.

```javascript
function rove([x,y,d], cmd) {
  var keys = Array.from(compass.keys());
  var [newPos] = keys.filter(([dir,deg]) => dir === d)
    .map(key => {
      var [dx,dy] = compass.get(key);
      var [dir, deg] = key;
      return commands[cmd]([x,y,deg],[dx,dy]);
    })
    .map(([x,y,deg]) => {
       var [newDir,newDeg] = keys.filter(([kdir,kdeg] => kdeg === deg % 36));
       return [x,y,newDir];
    });
  return newPos;
}
```
**BRAHMA** The whole thing looks like this:

```javascript
var compass = new Map([
    [['N',00], [0,1]],
    [['E',09], [1,0]],
    [['S',18], [0,-1]],
    [['W',27], [-1,0]]
]);

var commands = {
  'L': ([x,y,deg], [dx,dy]) => [x,y,deg-9],
  'R': ([x,y,deg], [dx,dy]) => [x,y,deg+9],
  'M': ([x,y,deg], [dx,dy]) => [x+dx,y+dy,deg]
};

function rove([x,y,d],cmd) {
  var keys = Array.from(compass.keys());
  var [newPos] = keys.filter(([dir,deg]) => dir === d)
    .map(key => {
      var [dx,dy] = compass.get(key);
      var [dir, deg] = key;
      return commands[cmd]([x,y,deg],[dx,dy]);
    })
    .map(([x,y,deg]) => {
      var [[newDir, newDeg]] = keys.filter(([kdir,kdeg]) => kdeg === deg % 36);
      return [x,y,newDir];
    });
    return newPos;
}

var p1 = rove([3,3,'E'], 'M');
var p2 = rove(p1, 'R');
var p3 = rove(p2, 'M');
var p4 = rove(p3, 'L');
console.info(p4);  // [4,2,'E']
```

**BRAHMA**  So, here everything is simply data and function, we did not require ```MarsRover``` or ```Vector``` abstractions, the essence has become more transparent and is there for anyone to see.  So Krishna, how does this look in an Array-Oriented Paradigm like APL?

**KRISHNA**  I am going to base my solution on the same fundamental data, tuples of movements for each axis, but rather than "abstract" this into a map, I will just use two simple arrays, a list of four directions and a 2-column matrix with one row per direction, defining the movement along each axis that the direction represents.

```apl
  ⎕IO←0    ⍝ Default index origin in APL is 1; we prefer 0
  directions←'NESW'
  movement←4 2⍴0 1,1 0,0 ¯1,¯1 0 ⍝ One row per direction (NESW)   
```

**KRISHNA** For experimentation, the initial conditions can be stored in 3 arrays as follows:

```apl
  position←3 3  ⍝ intial position
  heading←1     ⍝ index into directions
  commands←'MMRMMLMRML'
```

**KRISHNA** I chose to to represent the current direction as an index into the "direction" array. The directions are in order if you rotate right (clockwise), which means that our three possible commands L, M and R will result in a change in the direction index of ¯1, 0 and 1, respectively. We can compute the change in direction using the ```⍳``` function to produces the indices into 'LMR' of our command stream, from which we then subtract 1, giving ¯1 for L, 0 for M and 1 or R:

```apl
  ¯1+'LMR'⍳commands
0 0 1 0 0 ¯1 0 1 0 ¯1
```

**KRISHNA** This allows us to compute the direction at the end of each move by doing a sum scan (```+\```) of the starting position followed by a vector in which the commands have been mapped to the directional change that they produce:

```apl
  ⎕←bearings←4| +\ heading, ¯1+'LMR'⍳commands
1 1 1 2 2 2 1 1 2 2 1
```

**KRISHNA** The ```4|``` gives us the result modulus 4, ensuring that we wrap around nicely from W to N or vice versa and that ```bearings``` is now an array of integers between 0 and 3. If we now index the ```movement``` array by the bearings and mask this out by *multiplying* each row by 1 where the command is M and 0 elsewhere, we get a list of movements made by the robot.

```apl
  movements←movement[¯1↓bearings;] ×[0] commands='M'
  commands,movements ⍝ catenate commands to matrix of movements
M 1  0
M 1  0
R 0  0
M 0 ¯1
M 0 ¯1
L 0  0
M 1  0
R 0  0
M 0 ¯1
L 0  0
```
**KRISHNA** ```×[0]``` (multiply on leading axis) multiplies each row (pair of movements) on the left by each element of the Boolean vector on the right. The right argument to multiplication contains 1 for each M and 0 for all other commands, and as a result each R or L has resulted in a movement of (0 0) above. Now, I can compute the final position by doing a plus reduction on the starting position followed by the list of movements:

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
   rove←{⎕IO←0 
         directions←'NESW'                    ⍝ Clockwise from N
         movement←4 2⍴0 1,1 0,0 ¯1,¯1 0       ⍝ One row per direction (NESW)    
         (position heading commands)←⍵        ⍝ Deconstruct right argument
         direction←directions⍳heading          ⍝ Direction index
         bearings←4|+\direction,¯1+'LMR'⍳commands   ⍝ Bearing after each command
         moves←movement[¯1↓bearings;] ×[0] commands='M' ⍝ Movement resulting from each command
         (+⌿position⍪moves),directions[¯1↑bearings] ⍝ Return final position and bearing
        }

```

**KRISHNA** We could call it as follows:
```apl
  rove (3 3) 'E' 'MMRMMLMRML'
6 0 E
```

**KRISHNA** Note that there are no conditionals and no loops in the above function. the operations are applied to dense arrays of small integers or chacters. The  three steps which would be "switches" in most languages are implemented by 

1. translating command letters LMR into numbers representing the rotation (¯1 0 1)
2. multiplying movements in the current direction by 0 if the commands is not M
3. indexing into a movement array using the direction at the end of each command

This means that the function will be efficient even when executed by an interpreter, and also that it can be compiled for highly data parallel execution if suitable hardware is available.

**BRAHMA** Let me re-write this in JavaScript again.  So, I don't need ```Vector``` or ```Direction``` abstractions this time, a simple Array holding the movement tuples would do.  The indices of the array represent directions ```1``` for ```N```, ```2``` for ```E```, ```3``` for ```S``` and ```4``` for ```W```.

```javascript
function MarsRover(x, y, dirString) {
  let directions = ['N', 'E', 'S', 'W'];
  let directionIdx = Math.max(0, directions.indexOf(dirString));

  let point = [x, y];
  let movements = [[0,1], [1,0], [0,-1], [-1,0]];
  let toIndex = directionValue => Math.abs(directionValue % 4)
  
  let commands = {
    'M': (point, directionIdx) => [point.map((value, idx) => value + movements[directionIdx][idx]), directionIdx],
    'L': (point, directionIdx) => [point, directionIdx - 1],
    'R': (point, directionIdx) => [point, directionIdx + 1]
  };
  
  this.rove = command => {
    [point, directionIdx] = commands[command](point, directionIdx);
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

**BRAHMA** In OO paradigm, we first organize data and related behaviour together, discover or arrive at a suitable data-structure for the problem by the act of continuous refactoring.  If one pays attention to that aspect, then it leads to optimisation of the data-structure for a particular behaviour or a set of behaviours for an object.

**BRAHMA** The FP paradigm embraces transparency of the data-structure but makes the structure immutable, so one need not worry about an inadvertent change to the values.  Again, here as in OO, one has to arrive at the correct data-structure suitable for the problem at hand.

**BRAHMA** In the AO paradigm, like APL or J, the data structure is typically designed to make the information easily accessible, rather than being designed to allow a set of predicted behaviours. The choice of data structure may affect the elegance of the code - but APL's array operations like inner/outer product, transpose, reductions and scans along selected dimensions can be easily applied along any axis of arrays of any rank, shape or depth. For very large data volumes, the structure can influence performance and refactoring of the structure (and therefore the code) may become necessary.

**KRISHNA** Indeed. For example, with the array representation is it easy to perform "ad hoc" analyses like "how much time did the robot spend pointing in each compass direction?" (6 cycles pointing East, 5 pointing South).

```apl
     bearings←4| +\ heading, ¯1+'LMR'⍳commands ⍝ recompute bearings from commmands
     {⍺,≢⍵}⌸bearings                         ⍝ distinct bearings and counts
1 6
2 5
```

**KRISHNA** AO encourages you to think about the problem mathematically using numbers and arrays, and select array representations which embed or encode mathematical properties of the data - and where selection is computable rather than the result of a search. For example, this encourages you to realise that the sequence NWSE represents clockwise rotations of π/2, and that the L/R commands simply adjust the index by 1. This does mean that feeling comfortable with basic mathematics is a significant factor for an APL programmer.

**KRISHNA** In the original JavaScript, the abstractions actually lead the programmer away from the simplification that is available through mathematical insight into the problem. Even the map is problematic from this perspective. "Sound" programming principles of abstraction, aimed at reducing code complexity, have been applied. The solutions are more general, but also more complex, both for the human reader and the language engine.

**BRAHMA** So... is it correct to say that, the benefits of AO are most apparent when exploring new data, or faced with a project where significant parts of the requirement are in a state of flux? The freedom offered by arrays and being able to avoid abstraction early in a project will make new insights and the discovery of new algorithms and potential optimisations more likely.

**BRAHMA** But isn't there a risk that the lack of abstractions can break down? For example, if our robot needs behaviours that require complex state and interaction with the its environment, it might be hard to find a pure array representation with proper separation of information. At that point, I would expect that an object oriented decomposition of the problem - or other abstractions - to make the code easier to understand and maintain.

**KRISHNA** I think you are right... As complexity increases, it is important to realise where to draw the line and introduce abstractions. In fact, some people use APL as a design tool to explore the problem space, before locking things down in another language which is used for deployment - or as a tool to write tests in, to verify production application code is computing things correctly.

**BRAHMA** The final melody of our Jugalbandi should offer some further insight into this contrast - the choice between Transparency or Abstraction, and Concision or Verbosity?  Lets move on.