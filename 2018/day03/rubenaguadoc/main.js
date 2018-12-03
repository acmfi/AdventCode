const input =
require('./input').input
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


// 1 && 2

let grid = new Set();
let common = new Set();
let nOverlap = new Set();
let ids = {};
let acc = 0;

Array.prototype.forEach.call(input, e => {
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
