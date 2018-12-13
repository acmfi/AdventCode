let track = require('fs').readFileSync('./input.txt').toString().split(/\n/).map(e => e.match(/[\s\S]{1}/g) || []);

let carts = new Array();
let id = 0;

track.forEach((row, y) => row.forEach((elem, x) => {
  let newCart;
  if (elem === '>') {
    newCart = {
      heading: 'right',
      over: '-'
    };
  } else if (elem === '<') {
    newCart = {
      heading: 'left',
      over: '-'
    };
  } else if (elem === '^') {
    newCart = {
      heading: 'up',
      over: '|'
    };
  } else if (elem === 'v') {
    newCart = {
      heading: 'down',
      over: '|'
    };
  }

  if (newCart) {
    newCart.id = id;
    newCart.x = x;
    newCart.y = y;
    newCart.state = 0;
    carts.push(newCart);
    track[y][x] = newCart;
    id++;
  }
}));

function sortCarts (a, b) {
  if (a.y > b.y) {
    return 1;
  } else if (a.y === b.y) {
    return a.x > b.x;
  }
  return -1;
}

const directions = {
  '\\': {
    'up': 'left',
    'down': 'right',
    'left': 'up',
    'right': 'down'
  },
  '/': {
    'up': 'right',
    'down': 'left',
    'left': 'down',
    'right': 'up'
  },
  '+': {
    'up': {
      0: 'left',
      1: 'up',
      2: 'right'
    },
    'down': {
      0: 'right',
      1: 'down',
      2: 'left'
    },
    'left': {
      0: 'down',
      1: 'left',
      2: 'up'
    },
    'right': {
      0: 'up',
      1: 'right',
      2: 'down'
    }
  }
};

while (carts.length !== 1) {
  carts.forEach(cart => {
    track[cart.y][cart.x] = cart.over;
    if (cart.heading === 'right' || cart.heading === 'left') {
      cart.x += cart.heading == 'right' ? 1 : -1;
    } else {
      cart.y += cart.heading == 'down' ? 1 : -1;
    }
    cart.over = track[cart.y][cart.x];
    track[cart.y][cart.x] = cart;
    if (typeof cart.over === 'object') {
      console.log(`Collision: ${cart.x},${cart.y}; Carts: ${cart.id}<==>${cart.over.id}`);
      track[cart.y][cart.x] = cart.over.over;
      // console.log(carts.map(e => e.id));
      carts.splice(carts.indexOf(cart.over), 1);
      carts.splice(carts.indexOf(cart), 1);
      // console.log(carts.map(e => e.id));
    } else if (cart.over === '+') {
      cart.heading = directions[cart.over][cart.heading][cart.state];
      cart.state = (cart.state + 1) % 3;
    } else if (cart.over !== '-' && cart.over !== '|') {
      cart.heading = directions[cart.over][cart.heading];
    }
  });
  carts.sort(sortCarts);
  // console.log(printTrack(track));
}

console.log(carts);

function printTrack(track) {
  return track.map(e => e.map(f => {
    if (typeof f === "object") {
      if (f.heading === 'up') {
        return '^';
      } else if (f.heading === 'down') {
        return 'v';
      } else if (f.heading === 'left') {
        return '<';
      } else {
        return '>';
      }
    }
    return f;
  }).join('')).join('\n') + '\n-----------------------------------------------------------------------------------';
}
