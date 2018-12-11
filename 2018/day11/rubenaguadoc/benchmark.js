const inpt = 1308;

let objCahe = {};
let cache = new Array(300);
for (let i = 0; i < 300; i ++) {
  cache[i] = new Array(300);
}

const cell = (x, y, serial=inpt) => Math.floor(((x + 10) * y + serial) * (x + 10) % 1000 / 100) - 5;
const cellCache = (x, y, serial=inpt) => cache[x - 1][y - 1] = cache[x - 1][y - 1] || Math.floor(((x + 10) * y + serial) * (x + 10) % 1000 / 100) - 5;
const cellObjCache = (x, y, serial=inpt) => cache[`${x - 1}, ${y - 1}`] = cache[`${x - 1}, ${y - 1}`] || Math.floor(((x + 10) * y + serial) * (x + 10) % 1000 / 100) - 5;

console.time('NoCache');
for (let x = 1; x < 300 - 3; x++) {
  for (let y = 1; y < 300 - 3; y++) {
    for (let s = 1; s <= 20; s++) {
      cell(x, y, s);
      cell(x, y, s);
      cell(x, y, s);
      cell(x, y, s);
      cell(x, y, s);
      cell(x, y, s);
      cell(x, y, s);
      cell(x, y, s);
      cell(x, y, s);
    }
  }
}
console.timeEnd('NoCache');

console.time('ArrayCache');
for (let x = 1; x < 300 - 3; x++) {
  for (let y = 1; y < 300 - 3; y++) {
    for (let s = 1; s <= 20; s++) {
      cellCache(x, y, s);
      cellCache(x, y, s);
      cellCache(x, y, s);
      cellCache(x, y, s);
      cellCache(x, y, s);
      cellCache(x, y, s);
      cellCache(x, y, s);
      cellCache(x, y, s);
      cellCache(x, y, s);
    }
  }
}
console.timeEnd('ArrayCache');

console.time('ObjCache');
for (let x = 1; x < 300 - 3; x++) {
  for (let y = 1; y < 300 - 3; y++) {
    for (let s = 1; s <= 20; s++) {
      cellObjCache(x, y);
      cellObjCache(x, y);
      cellObjCache(x, y);
      cellObjCache(x, y);
      cellObjCache(x, y);
      cellObjCache(x, y);
      cellObjCache(x, y);
      cellObjCache(x, y);
      cellObjCache(x, y);
    }
  }
}
console.timeEnd('ObjCache');

function gridVal (x, y, s=3) {
  let sum = 0;
  for (let dx = 0; dx < s; dx++) {
    for (let dy = 0; dy < s; dy++) {
      sum += cell(x + dx, y + dy);
    }
  }
  return sum;
}

function gridValH (x, y) {
  return cell(x, y) + cell(x + 1, y) + cell(x + 2, y) + cell(x, y + 1) + cell(x + 1, y + 1) + cell(x + 2, y + 1) + cell(x, y + 2) + cell(x + 1, y + 2) + cell(x + 2, y + 2);
}

console.time('For');
for (let x = 1; x < 300 - 3; x++) {
  for (let y = 1; y < 300 - 3; y++) {
    gridVal(x, y);
  }
}
console.timeEnd('For');

console.time('HardCoded');
for (let x = 1; x < 300 - 3; x++) {
  for (let y = 1; y < 300 - 3; y++) {
    gridValH(x, y);
  }
}
console.timeEnd('HardCoded');
