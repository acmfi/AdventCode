const input = require('fs').readFileSync('./input.txt').toString();
let status = {};
for (let i = 'A'.charCodeAt(0); i <= 'Z'.charCodeAt(0); i++) {
  status[String.fromCharCode(i)] = false;
}
const alphabet = Object.keys(status);

let restrictions = {};
const reg = /Step (.) must be finished before step (.) can begin.\n/g;
while (true) {
  const match = reg.exec(input);
  if (match == null) break;
  const from = match[1]
  const to = match[2];
  if (!restrictions[from]) {
    restrictions[from] = [to];
  } else {
    restrictions[from].push(to);
  }
}

// 1
let res = [];
while (!Object.values(status).reduce((a, b) => a && b)) {
  let abc = new Set(alphabet);
  Object.keys(restrictions).forEach(k => {
    if (!status[k]) {
      restrictions[k].forEach(step => abc.delete(step));
    }
  });
  let id;
  const it = abc.values();
  while (true) {
    id = it.next().value;
    if (!status[id]) break;
  }
  res.push(id);
  status[id] = true;
}

console.log(res.join(''));

// 2
let count = 0;
Object.keys(status).forEach(k => status[k] = false);
// DO NOT USE THIS WITH OBJECTS
// let workers = new Array(5).fill({ worked: 0, working: '' });
let workers = [
  { worked: 0, working: '' },
  { worked: 0, working: '' },
  { worked: 0, working: '' },
  { worked: 0, working: '' },
  { worked: 0, working: '' }
];

while (!Object.values(status).reduce((a, b) => a && b)) {
  workers = workers.map(worker => {
    if (worker.working) {
      worker.worked++;
      if (worker.worked == 60 + worker.working.charCodeAt(0) - 'A'.charCodeAt(0) + 1) {
        status[worker.working] = true;
        worker.working = '';
        worker.worked = 0;
      }
    }
    return worker;
  });
  let abc = new Set(alphabet);
  Object.keys(restrictions).forEach(k => {
    if (!status[k]) {
      restrictions[k].forEach(step => abc.delete(step));
    }
  });
  Object.keys(status).forEach(k => {
    if (status[k]) {
      abc.delete(k);
    }
  })
  workers.forEach(worker => abc.delete(worker.working));
  let it = abc.values();
  let el = it.next();
  while (!el.done) {
    workers.some((worker, i) => {
      if (!worker.working) {
        workers[i].working = el.value;
        return true;
      }
      return false;
    });
    el = it.next();
  }
  count++;
}

console.log(count - 1);
