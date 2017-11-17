function MarsRover(x, y, dirString) {
  const directions = ['N', 'E', 'S', 'W'];
  let dirIdx = Math.max(0, directions.indexOf(dirString));

  let point = [x, y];
  const movements = [[0,1], [1,0], [0,-1], [-1,0]];
  const toIndex = directionValue => Math.abs(directionValue % 4)
  
  const commands = {
    'M': (point, dirIdx) => [point.map((value, idx) => value + movements[dirIdx][idx]), dirIdx],
    'L': (point, dirIdx) => [point, dirIdx - 1],
    'R': (point, dirIdx) => [point, dirIdx + 1]
  };
  
  this.rove = command => {
    [point, dirIdx] = commands[command](point, dirIdx);
    return this;
  }
  
  this.toString = () => `${point[0]} ${point[1]} ${directions[toIndex(dirIdx)]}`
}

const bearing = [3,3,'E'];
console.info(['M','M','R','M','M','L','M','R','M', 'L']
  .reduce((b, c) => rove(b, c), bearing));