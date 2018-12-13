let track = require('fs').readFileSync('../skgsergio/input').toString().split(/\n/).map(e => e.match(/[\s\S]{1}/g) || []);

let carts = new Array();
let id = 0;

track.forEach((row, y) => row.forEach((elem, x) => {
  if (elem === '>' || elem === '<') {
    track[y][x] = '-';
  } else if (elem === '^' || elem === 'v') {
    track[y][x] = '|';
  } else {
    return;
  }
  carts.push({ id, x, y, heading: elem, state: 0 });
  id++;
}));


function printTrack(carts, track) {
  let t2 = track;
  carts.forEach(cart => t2[cart.y][cart.x] = cart.heading);
  return t2.map(e => e.join('')).join('\n') + '\n-------------------------------------------------------------------------------------------------------------------------------------';
}


function sortCarts (a, b) {
  if (a.y > b.y) {
    return 1;
  } else if (a.y === b.y) {
    return a.x > b.x ? 1 : 0;
  }
  return -1;
}

const directions = {
  '\\': {
    '^': '<',
    'v': '>',
    '<': '^',
    '>': 'v'
  },
  '/': {
    '^': '>',
    'v': '<',
    '<': 'v',
    '>': '^'
  },
  '+': {
    '^': ['<', '^', '>'],
    'v': ['>', 'v', '<'],
    '<': ['v', '<', '^'],
    '>': ['^', '>', 'v']
  }
};

while (carts.length > 1) {
  for (let i = 0; i < carts.length; i++) {
    let cart = carts[i];
    if (!cart) break;
    if (cart.heading === '>' || cart.heading === '<') {
      cart.x += cart.heading === '>' ? 1 : -1;
    } else {
      cart.y += cart.heading === 'v' ? 1 : -1;
    }

    for (let j = 0; j < carts.length; j++) {
      let c = carts[j];
      if (c.x === cart.x && c.y === cart.y && c.id !== cart.id) {
        console.log(`Collision: ${cart.y},${cart.x}`);
        carts.splice(i, 1);
        if (i < j) {
          j--;
        }
        carts.splice(j, 1);
        i -= 2;
        break;
      }
    }

    const over = track[cart.y][cart.x];
    if (over === '/' || over === '\\') {
      cart.heading = directions[over][cart.heading];
    } else if (over === '+') {
      cart.heading = directions[over][cart.heading][cart.state];
      cart.state = (cart.state + 1) % 3;
    }
  }
  carts.sort(sortCarts);
}

console.log(carts[0]);
