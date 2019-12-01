const addr = (reg, [a, b]) => reg[a] + reg[b];

const addi = (reg, [a, b]) => reg[a] + b;

const mulr = (reg, [a, b]) => reg[a] * reg[b];

const muli = (reg, [a, b]) => reg[a] * b;

const banr = (reg, [a, b]) => reg[a] & reg[b];

const bani = (reg, [a, b]) => reg[a] & b;

const borr = (reg, [a, b]) => reg[a] | reg[b];

const bori = (reg, [a, b]) => reg[a] | b;

const setr = (reg, [a]) => reg[a];

const seti = (_, [a]) => a;

const gtir = (reg, [a, b]) => a > reg[b] ? 1 : 0;

const gtri = (reg, [a, b]) => reg[a] > b ? 1 : 0;

const gtrr = (reg, [a, b]) => reg[a] > reg[b] ? 1 : 0;

const eqir = (reg, [a, b]) => a == reg[b] ? 1 : 0;

const eqri = (reg, [a, b]) => reg[a] == b ? 1 : 0;

const eqrr = (reg, [a, b]) => reg[a] == reg[b] ? 1 : 0;

module.exports = {
    addr,
    addi,
    mulr,
    muli,
    banr,
    bani,
    borr,
    bori,
    setr,
    seti,
    gtir,
    gtri,
    gtrr,
    eqir,
    eqri,
    eqrr
};