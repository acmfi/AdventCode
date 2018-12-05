const input = require('fs').readFileSync('./input.txt').toString().trim();

const compare = (c1, c2) => c1 && c2 && Math.abs(c1 - c2) == 32;

function react(txt) {
  for (let i = 0; i < txt.length; i++) {
    while (true) {
      if (!compare(txt.charCodeAt(i), txt.charCodeAt(i + 1))) break;
      txt = txt.slice(0, i) + txt.slice(i + 2);
      i--;
    }
  }
  return txt;
}

// 1
const reacted = react(input);
console.log(reacted.length);

// 2
let res = {};
for (let i = 'a'.charCodeAt(0); i <= 'z'.charCodeAt(0); i++) {
  res[ String.fromCharCode(i) ] = react(
    reacted
    .replace(new RegExp(String.fromCharCode(i), 'g'), '')
    .replace(new RegExp(String.fromCharCode(i - 32), 'g'), '')
  ).length;
}

console.log(
  Object.keys(res).reduce((a, b) => a < res[b] ? a : res[b])
);
