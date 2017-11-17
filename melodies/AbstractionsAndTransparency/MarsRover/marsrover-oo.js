function MarsRover(x, y, dirString) {
  var vector = new Vector(x, y, dirString);

  const commands = {
    'M': vector => vector.moveForward(),
    'L': vector => vector.turnLeft(),
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
    'N' : ['W', (x,y) => [x, y+1], 'E'], 
    'E' : ['N', (x,y) => [x+1, y], 'S'], 
    'S' : ['E', (x,y) => [x, y-1], 'W'], 
    'W' : ['S', (x,y) => [x-1, y], 'N'] 
  };

  this.turnLeft = () => new Vector(x, y, directions[direction][0]);
  
  this.moveForward = () => {
    const [newX, newY] = directions[direction][1](x, y);
    return new Vector(newX, newY, direction);
  };
  
  this.turnRight = () => new Vector(x, y, directions[direction][2]);
  
  this.toString = () => `${x} ${y} ${direction}`;
}

const rover = new MarsRover(3, 3, 'E');
console.info(rover.rove('M').rove('R').rove('M').rove('L').toString());
