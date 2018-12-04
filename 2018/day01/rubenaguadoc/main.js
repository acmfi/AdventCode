const input = require('fs').readFileSync('./input.txt').toString().trim().split(/\n/).map(e => parseInt(e));

// Part 1
console.log(input.reduce((a, b) => a + b));

// Part 2
let history = new Set();
let found = false;
let acc = 0;

while (!found) {
  found = input.some(e => {
    history.add(acc);
    acc += e;
    return history.has(acc);
  });
}

console.log(acc);
