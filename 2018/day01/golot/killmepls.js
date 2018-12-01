//1
eval(document.body.firstElementChild.textContent.split("\n").reduce((a, x) => a+x))

//2
var f = new Set();
var r = NaN;
var sum = 0;
while(isNaN(r)) document.body.firstElementChild.textContent.split("\n").forEach(function(x) { n = parseInt(x); if(isNaN(n) || !isNaN(r)) return; sum+=n; if(f.has(sum)) r = sum; else f.add(sum); })