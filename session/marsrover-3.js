function MarsRover(x, y, dirString) {
  var vector = new Vector(x, y, dirString);

  const commands = {
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
  const directions = {
    'N' : ['W', 'E', (x,y) => [x, y+1]],
    'E' : ['N', 'S', (x,y) => [x+1, y]],
    'S' : ['E', 'W', (x,y) => [x, y-1]],
    'W' : ['S', 'N', (x,y) => [x-1, y]]
  };

  this.moveForward = () => {
    const [newX, newY] = directions[direction][2](x, y);
    return new Vector(newX, newY, direction);
  };

  this.turnLeft = () => new Vector(x, y, directions[direction][0]);
  this.turnRight = () => new Vector(x, y, directions[direction][1]);
  this.toString = () => `${x} ${y} ${direction}`;
}

const rover = new MarsRover(3, 3, 'E');
console.info(rover.rove('M').rove('R').rove('M').rove('L').toString());
