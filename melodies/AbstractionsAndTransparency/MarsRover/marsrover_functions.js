var compass = new Map([
    [['N',00], [0,1]],
    [['E',09], [1,0]],
    [['S',18], [0,-1]],
    [['W',27], [-1,0]]
]);

var commands = {
  'L': ([x,y,deg], [dx,dy]) => [x,y,deg-9],
  'R': ([x,y,deg], [dx,dy]) => [x,y,deg+9],
  'M': ([x,y,deg], [dx,dy]) => [x+dx,y+dy,deg]
};

function rove([x,y,d],cmd) {
  var keys = Array.from(compass.keys());
  var [newPos] = keys.filter(([dir,deg]) => dir === d)
    .map(key => {
      var [dx,dy] = compass.get(key);
      var [dir, deg] = key;
      return commands[cmd]([x,y,deg],[dx,dy]);
    })
    .map(([x,y,deg]) => {
      var [[newDir, newDeg]] = keys.filter(([kdir,kdeg]) => kdeg === deg % 36);
      return [x,y,newDir];
    });
    return newPos;
}

var p1 = rove([3,3,'E'], 'M');
console.info(p1);
var p2 = rove(p1, 'R');
console.info(p2);
var p3 = rove(p2, 'M');
console.info(p3);
var p4 = rove(p3, 'L');
console.info(p4);
