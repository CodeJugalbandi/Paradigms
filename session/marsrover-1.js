function MarsRover(x, y, direction) {
  this.rove = (command) => this;
  this.toString = () => `${x} ${y} ${direction}`
}

const rover = new MarsRover(3, 3, 'E');
console.info(rover.rove('M').rove('R').rove('M').rove('L').toString());