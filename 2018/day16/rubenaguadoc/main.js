const input = require('fs').readFileSync('./input.txt').toString();
const ops = require('./instructions');
const merge = (a, b) => a.filter(e => b.includes(e));

const re = /Before: \[(\d*), (\d*), (\d*), (\d*)\]\n(\d*) (\d*) (\d*) (\d*)\nAfter:\s{2}\[(\d*), (\d*), (\d*), (\d*)\]/g;
const is = [];
let match = re.exec(input);
while (match != null) {
    match = match.map(e => parseInt(e));
    is.push([[match[1], match[2], match[3], match[4]], [match[5], match[6], match[7], match[8]], [match[9], match[10], match[11], match[12]]]);
    match = re.exec(input);
}

// Star 1
let cops = {};
let howMany = 0;
is.forEach(i => {
    const posibleCop = [];
    const cop = i[1][0];
    Object.values(ops).forEach((op, idop) => {
        const res = i[0].slice();
        res[i[1][3]] = op(i[0], i[1].slice(1, 3));
        if ( res.every((v, id) => v === i[2][id]) ) posibleCop.push(idop); 
    });
    if (posibleCop.length >= 3) howMany++;
    cops[cop] = cops[cop] ? merge(cops[cop], posibleCop) : posibleCop;
});
console.log(`P1: ${howMany}`);


cops = Object.values(cops).map(v => v);
while(cops.filter(e => typeof e === 'object').length !== 0) {
    cops.forEach((e, i) => {
        if (e.length == 1) {
            cops[i] = e[0];
            cops.forEach((e2, i2) => {
                if (typeof e2 == 'object') {
                    cops[i2] = e2.filter(e3 => e3 !== e[0]);
                }
            });
        }
    });
}

// Star 2
const regs = [0, 0, 0, 0];
const re2 = /(\d*) (\d*) (\d*) (\d*)/g;

const inpt2 = input.split(/\n\n\n\n/)[1];
match = re2.exec(inpt2);
while (match != null) {
    match = match.map(e => parseInt(e));
    regs[match[4]] = Object.values(ops)[cops[match[1]]](regs, [match[2], match[3]]);
    match = re2.exec(inpt2);
}
console.log(`P2: ${regs[0]}`);