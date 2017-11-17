function MarsRover(x, y, dirString) {
  var vector = new Vector(x, y, dirString);

  const commands = {
    'L': vector => vector.turnLeft(),
    'M': vector => vector.moveForward(),
    'R': vector => vector.turnRight()
  };
  
  this.rove = cmd => {
    vector = commands[cmd](vector);
    return this;
  }

  this.toString = () => vector.toString();
}

function Vector(x, y, direction) {
  const directions = {
    'N' : 'W',
    'E' : 'N',
    'S' : 'E',
    'W' : 'S'
  };

  this.turnLeft = () => new Vector(x, y, directions[direction]);
  this.moveForward = () => this;
  this.turnRight = () => this;
  this.toString = () => `${x} ${y} ${direction}`;
}

const rover = new MarsRover(3, 3, 'E');
console.info(rover.rove('M').rove('R').rove('M').rove('L').toString());
