const compass = new Map([
    [['N',00], [0,1]],
    [['E',09], [1,0]],
    [['S',18], [0,-1]],
    [['W',27], [-1,0]]
]);

const commands = {
  'L': ([x,y,deg], [dx,dy]) => [x,y,deg-9],
  'R': ([x,y,deg], [dx,dy]) => [x,y,deg+9],
  'M': ([x,y,deg], [dx,dy]) => [x+dx,y+dy,deg]
};

function rove([x,y,d],cmd) {
  const keys = Array.from(compass.keys());
  const [newPos] = keys.filter(([dir,deg]) => dir === d)
    .map(key => {
      const [dx,dy] = compass.get(key);
      const [dir, deg] = key;
      return commands[cmd]([x,y,deg],[dx,dy]);
    })
    .map(([x,y,deg]) => {
      const [[newDir, newDeg]] = keys.filter(([kdir,kdeg]) => kdeg === deg % 36);
      return [x,y,newDir];
    });
    return newPos;
}

const p1 = rove([3,3,'E'], 'M');
const p2 = rove(p1, 'R');
const p3 = rove(p2, 'M');
const p4 = rove(p3, 'L');
console.info(p4);
