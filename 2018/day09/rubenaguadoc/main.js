// 413 players; last marble is worth 71082 points
console.time('a');
const players = 413;
const lastMarble = 71082 * 100;

let score = {};
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
console.timeEnd('a');
