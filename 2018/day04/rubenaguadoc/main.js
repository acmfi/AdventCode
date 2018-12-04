const input = require('fs').readFileSync('./input.txt').toString().trim().split(/\n/).sort();

let acc = {};
let lastId, sleepingSince;

input.forEach(e => {
  const match = /\[\d{4}\-\d{2}\-\d{2} \d{2}\:(\d{2})\] (.*)/.exec(e);
  const id = /.*\#(\d+).*/.exec(match[2]);
  if (id !== null) {
    lastId = id[1];
  } else if (match[2] === 'falls asleep') {
    sleepingSince = parseInt(match[1]);
  } else {
    if(!acc[lastId]) acc[lastId] = { acc: 0, hours: {} };
    const sleepingTo = parseInt(match[1]);
    acc[lastId].acc += sleepingTo - sleepingSince;
    for (let i = sleepingSince; i < sleepingTo; i++) {
      const pre = acc[lastId].hours[i];
      acc[lastId].hours[i] = (pre?pre:0) + 1;
    }
  }
});

const max = obj => Object.keys(obj).reduce((a, b) => obj[a] > obj[b] ? a : b);

// 1
const sleepest = Object.keys(acc).reduce((a, b) => acc[a].acc > acc[b].acc ? a : b);
console.log(sleepest * max(acc[sleepest].hours));


// 2
Object.keys(acc).forEach(e => {
  const maxh = max(acc[e].hours);
  acc[e] = { h: maxh, q: acc[e].hours[maxh] };
});
const elMasVago = Object.keys(acc).reduce((a, b) => acc[a].q > acc[b].q ? a : b);
console.log(elMasVago * acc[elMasVago].h);
