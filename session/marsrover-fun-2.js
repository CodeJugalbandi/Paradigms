const compass = new Map([
  [['N',00], [0, 1]],
  [['E',09], [1, 0]],
  [['S',18], [0, -1]],
  [['W',27], [-1, 0]]
]);

function rove([x,y,d],cmd) {
  const keys = Array.from(compass.keys());
  const [newPos] = keys.filter(([dir,deg]) => dir === d)
  return newPos;
}

const p1 = rove([3,3,'E'], 'M');
console.info(p1);