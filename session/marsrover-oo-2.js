function MarsRover(x, y, direction) {
  var vector = new Vector(x, y, direction);

  const commands = { 
    'L': vec => vec.turnLeft(),
    'M': vec => vec,
    'R': vec => vec
  };
  
  this.rove = cmd => {
    vector = commands[cmd](vector);
    return this;
  }

  this.toString = () => `${x} ${y} ${direction}`
}

function Vector(x, y, direction) {
  this.turnLeft = () => this; 

  this.toString = () => `${x} ${y} ${direction}`;
}

const rover = new MarsRover(3, 3, 'E');
console.info(rover.rove('M')
                .rove('R')
                .rove('M')
                .rove('L')
                .toString());