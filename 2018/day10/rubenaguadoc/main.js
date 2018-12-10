const input = window.input;

let points = [];
const reg = /position=<(.\d*), (.\d*)> velocity=<(.\d*), (.\d*)>\n/g;
while (true) {
  const match = reg.exec(input);
  if (match == null) break;
  points.push({
    x: parseInt(match[1]),
    y: parseInt(match[2]),
    vx: parseInt(match[3]),
    vy: parseInt(match[4])
  });
}

window.onload = async _ => {
  let i = 1;
  while (step(points, 100)) {
    await wait(1);
    i++;
  }
  step(points, 42)
  console.log(i * 100 + 42);
};

function wait(t) {
  return new Promise(resolve => setTimeout(resolve, t));
}

function step(points, factor) {
  let x = [];
  let y = [];
  points.forEach(e => {
    e.x += factor * e.vx;
    e.y += factor * e.vy;
    x.push(e.x);
    y.push(e.y);
  });
  Plotly.newPlot('sky', [{
    x,
    y,
    type: 'scatter',
    mode: 'markers',
  }]);

  if (points.reduce((a, b) => a + Math.abs(b.x) + Math.abs(b.y), 0) < 130000) {
    return false;
  }
  return true;
  // if (points.reduce((a, b) => a + Math.abs(b.x) + Math.abs(b.y), 0) < 120000) {
  //   t = 0.001;
  // }
  // if ((new Set(points.map(e => e.y))).length > 100) break;
}
