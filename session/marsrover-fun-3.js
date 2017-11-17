const compass = new Map([
  [['N',00], [0, 1]],
  [['E',09], [1, 0]],
  [['S',18], [0, -1]],
  [['W',27], [-1, 0]]
]);

const commands = new Map([
  ['L', (dx,dy) => [0,0,-9]],
  ['R', (dx,dy) => [0,0,9]],
  ['M', (dx,dy) => [dx,dy,0]]
]);

function rove([x,y,d],cmd) {
  const keys = Array.from(compass.keys());
  const [newPos] = keys.filter(([dir,deg]) => dir === d)
            .map(key => {
              const [dx,dy] = compass.get(key);
              const [dir, deg] = key;
              const [newDx, newDy,newDeg] = commands.get(cmd)(dx,dy);
              return [x+newDx, y+newDy, deg+newDeg];
            });
  return newPos;
}

const p1 = rove([3,3,'E'], 'M');
console.info(p1);