// 413 players; last marble is worth 71082 points
const players = 413;
const lastMarble = 71082;

let score = {};

console.time('Array');
let circle = [0];
let currentMarble = 0;

for (let i = 1; i <= lastMarble; i++) {
  // console.log(i);
  if (i % 23 != 0) {
    currentMarble = (currentMarble + 1) % circle.length + 1;
    circle.splice(currentMarble, 0, i);
  } else {
    let currentPlayer = ((i - 1) % players) + 1;
    currentMarble = (currentMarble + circle.length - 7) % circle.length;
    score[currentPlayer] = (score[currentPlayer] || 0) + i + circle.splice(currentMarble, 1)[0];
  }
}

console.log(
  Object.values(score).reduce((a, b) => a > b ? a : b)
);
console.timeEnd('Array');


const { Element, LinkedList } = require('./LinkedList');
score = {};
console.time('LinkedList');
circle = new LinkedList(new Element(0));

for (let i = 1; i <= lastMarble; i++) {
  if (i % 23 != 0) {
    circle.rol();
    circle.push(new Element(i));
  } else {
    let currentPlayer = ((i - 1) % players) + 1;
    circle.ror(7);
    score[currentPlayer] = (score[currentPlayer] || 0) + i + circle.pop().value();
    circle.rol();
  }
}

console.log(
  Object.values(score).reduce((a, b) => a > b ? a : b)
);
console.timeEnd('LinkedList');
