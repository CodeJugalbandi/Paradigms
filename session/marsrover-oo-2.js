function MarsRover(x, y, direction) {
  var _x = x;
  var _y = y;
  var _direction = direction;
  
  const commands = { 
    'L': (x,y,d) => {
       switch(d) {
         case 'N' : return [x,y,'W'];
         case 'E' : return [x,y,'N'];
         case 'S' : return [x,y,'E'];
         case 'W' : return [x,y,'S'];
       }
     },
    'M': (x,y,d) => {
       switch(d) {
         case 'N' : return [x,y+1,d];
         case 'E' : return [x+1,y,d];
         case 'S' : return [x,y-1,d];
         case 'W' : return [x-1,y,d];
       }
    },
    'R': (x,y,d) => [x,y,d]
  };
  
  this.rove = cmd => {
    [_x, _y, _direction] = commands[cmd](_x, _y, _direction);
    return this;
  }

  this.toString = () => `${_x} ${_y} ${_direction}`
}

const rover = new MarsRover(3, 3, 'E');
console.info(rover.rove('M')
                .rove('R')
                .rove('M')
                .rove('L')
                .toString());