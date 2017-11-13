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
