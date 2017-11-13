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