const input = require('fs').readFileSync('./input.txt').toString().trim().split(/\n/);
// const input = require('fs').readFileSync('./testInput.txt').toString().trim().split(/\n/);

let initialState = input[0].replace('initial state:', '').trim();
let transformations = {};
input.slice(2).forEach(e => {
  const parts = e.split(' => ');
  transformations[parts[0]] = parts[1];
});
let zero = 0;
let history = new Set();
let historyArr = new Array();
let exit = false;

// Chage 50000000000 <=> 20
for (let generation = 0; generation < 50000000000 && !exit; generation++) {
  while (initialState.indexOf('#') < 5) {
    initialState = '.' + initialState;
    zero++;
  }
  while (initialState.lastIndexOf('#') > initialState.length - 6) {
    initialState = initialState + '.';
  }
  let newState = initialState;
  for (let pos = 0; pos < initialState.length; pos++) {
    let chunk = initialState.substring(pos - 2, pos + 3);
    const newCentral = transformations[chunk];
    if (newCentral) {
      newState = newState.substring(0, pos) + newCentral + newState.substring(pos + 1);
    }
  }
  initialState = newState.replace(/^\.*/, '').replace(/\.*$/, '');
  zero -= newState.indexOf('#');
  historyArr.push({ zero, initialState });
  if (history.has(initialState)) {
    exit = true;
    let whereStarts = historyArr.findIndex(e => e.initialState === initialState);
    initialState = historyArr[whereStarts + (50000000000 - generation) % (historyArr.length - whereStarts)].initialState;
    zero += (zero - historyArr[whereStarts].zero) * (50000000000 - generation) + 1;
    break;
  }
  history.add(initialState);
}

console.log(
  initialState.split('').reduce((a, b, i) => a + (b == '#' ? i - zero : 0), 0)
);
