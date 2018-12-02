const input = require('./input').input.split(/\n/);

// 1
let count = new Array();
input.forEach((e, i) => {
  let subcount = {};
  for (let j = 0; j < e.length; j++) {
    subcount[e.charAt(j)] = 1 + (subcount[e.charAt(j)] || 0);
  }
  count[i] = subcount;
});

const reducer = (a, b) => {
  a[0] += b[0];
  a[1] += b[1];
  return a;
};

count = count
.map(e => [Object.values(e).includes(2), Object.values(e).includes(3)])
.reduce(reducer, [0, 0])
.reduce((a, b) => a * b);

console.log(count);

// 2
input.some((e, i) => {
  for (let j = 0; j < e.length; j++) {
    let regex = new RegExp(`^${e.substr(0, j)}.${e.substr(j + 1)}$`);
    if (input.find((v, id) => regex.test(v) && id !== i)) {
      console.log(e.slice(0, j) + e.slice(j + 1));
      return true;
    }
  }
});
