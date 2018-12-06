let maxx, maxy;
const input = require('fs').readFileSync('./input.txt').toString().trim().split(/\n/).map(e => {
  const coord = e.split(', ');
  const x = parseInt(coord[0]);
  const y = parseInt(coord[1]);
  maxx = maxx > x ? maxx : x;
  maxy = maxy > y ? maxy : y;
  return { x, y };
});

let acc = new Array(input.length);
let excluded = [];
let acc2 = 0;

for (let i = 0; i <= maxx; i++) {
  for (let j = 0; j <= maxy; j++) {
    const distances = input.map(e => Math.abs(e.x - i) + Math.abs(e.y - j));
    if (distances.reduce((a, b) => a + b) < 10000) acc2 += 1;
    const minDist = Math.min(...distances);
    if (distances.filter(e => e == minDist).length == 1) {
      const id = distances.indexOf(minDist);
      acc[id] = 1 + (acc[id] ? acc[id] : 0);
      if (i == 0 || j == 0 || i == maxx || j == maxy) {
        excluded.push(id);
      }
    }
  }
}

excluded.forEach(e => acc[e] = -1);

console.log(Math.max(...acc), acc2);
