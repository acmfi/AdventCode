const inpt = require('fs').readFileSync('./input.txt').toString().trim();

console.time('Bucle');
const inpt1 =
inpt
.split(/\n/)
.map(e => e.split(' '))
.map(e =>
  ({
    id: parseInt(e[0].replace('#', '')),
    x: parseInt(e[2].split(',')[0]),
    y: parseInt(e[2].split(',')[1].replace(':', '')),
    w: parseInt(e[3].split('x')[0]),
    h: parseInt(e[3].split('x')[1])
  })
);
console.timeEnd('Bucle');


console.time('Regex');
const reg = /#(\d+) @ (\d+),(\d+): (\d+)x(\d+)\n/g;
let inpt2 = [];
while (true) {
  let match = reg.exec(inpt);
  if (match == null) break;
  inpt2.push({
    id: parseInt(match[1]),
    x: parseInt(match[2]),
    y: parseInt(match[3]),
    w: parseInt(match[4]),
    h: parseInt(match[5])
  });
}
console.timeEnd('Regex');

// 1 && 2

let grid = new Set();
let common = new Set();
let nOverlap = new Set();
let ids = {};
let acc = 0;

inpt1.forEach(e => {
  let overlap = false;
  for (let i = 0; i < e.w * e.h; i++) {
    const compound = `${e.x + i % e.w},${e.y + Math.floor(i / e.w)}`;
    if (grid.has(compound)) {
      overlap = true;
      nOverlap.delete(ids[compound]);
      if (!common.has(compound)) {
        acc += 1;
        common.add(compound)
      }
    } else {
      grid.add(compound);
      ids[compound] = e.id;
    }
  }
  if (!overlap) {
    nOverlap.add(e.id);
  }
});

console.log(acc, nOverlap);
