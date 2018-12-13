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
    'up': ['left', 'up', 'right'],
    'down': ['right', 'down', 'left'],
    'left': ['down', 'left', 'up'],
    'right': ['up', 'right', 'down']
  }
};

while (carts.length !== 1) {
  let collided = [];
  carts.forEach(cart => {
    // console.log(typeof cart.over == 'object' ? cart.over : '');
    track[cart.y][cart.x] = cart.over;
    if (cart.heading === 'right' || cart.heading === 'left') {
      cart.x += cart.heading == 'right' ? 1 : -1;
    } else {
      cart.y += cart.heading == 'down' ? 1 : -1;
    }
    cart.over = track[cart.y][cart.x];
    track[cart.y][cart.x] = cart;
    if (typeof cart.over === 'object') {
      if (carts.indexOf(cart.over) != -1) {
        console.log(`Collision: ${cart.x},${cart.y}; Carts: ${cart.id} ==> ${cart.over.id}`);
        // if (cart.id == 16) {
        // console.log(printTrack(track));
        // }
        // track[cart.y][cart.x] = cart.over.over;
        // if (cart.id == 16) {
        // console.log(printTrack(track));
        // }
        collided.push(cart);
      } else {
        console.log('Fake');
      }
    } else if (cart.over === '+') {
      cart.heading = directions[cart.over][cart.heading][cart.state];
      cart.state = (cart.state + 1) % 3;
    } else if (cart.over !== '-' && cart.over !== '|') {
      cart.heading = directions[cart.over][cart.heading];
    }
  });
  if (collided.length !== 0) {
    collided.forEach(cart => {
      console.log(cart);
      let a = cart;
      while (typeof a === 'object') {
        a = a.over;
      }
      track[cart.y][cart.x] = cart.over;
      console.log(carts.map(e => e.id));
      carts.splice(carts.indexOf(cart.over), 1);
      carts.splice(carts.indexOf(cart), 1);
      console.log(carts.map(e => e.id));
    });
    console.log(track.reduce((a, b) => a + b.reduce((c, d) => typeof d == 'object' ? ++c : c, 0), 0));
  }
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
