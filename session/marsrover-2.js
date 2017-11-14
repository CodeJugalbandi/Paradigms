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