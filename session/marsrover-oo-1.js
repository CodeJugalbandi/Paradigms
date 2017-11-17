function MarsRover(x, y, direction) {
  var _x = x;
  var _y = y;
  var _direction = direction;
  
  this.rove = cmd => this;
  this.toString = () => `${_x} ${_y} ${_direction}`;
}

const rover = new MarsRover(3, 3, 'E');
console.info(rover.rove('M')
                .rove('R')
                .rove('M')
                .rove('L')
                .toString());