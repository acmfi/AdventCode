const input = require('fs').readFileSync('./input.txt').toString().trim().split(/\s/).map(e => parseInt(e));

// 1 + 2
function sumNode(chain, index) {
  const [ nChilds, nMetadata ] = [ chain[index.i++], chain[index.i++] ];
  let [ sum1, sum2, childrenValues ] = [ 0, 0, [] ];
  for (let i = 0; i < nChilds; i++) {
    const [ s1, s2 ] = sumNode(chain, index);
    sum1 += s1;
    childrenValues.push(s2);
  }
  for (let i = 0; i < nMetadata; i++) {
    const val = chain[index.i++];
    sum1 += val;
    sum2 += nChilds == 0 ? val : (childrenValues[val - 1] || 0);
  }
  return [ sum1, sum2 ];
}

sumNode(input, { i: 0 }).forEach(e => console.log(e));
