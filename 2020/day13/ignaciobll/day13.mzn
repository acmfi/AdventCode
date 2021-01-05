% Reccomended to solve with OptiMathSAT
% minizinc -c day13.mzn
% optimathsat -input=fzn < day13.fzn
var int: t;
var int: b41;
var int: b37;
var int: b557;
var int: b29;
var int: b13;
var int: b17;
var int: b23;
var int: b419;
var int: b19;

constraint t >= 0;
constraint b41 = t;
constraint b41 mod 41 = 0;

constraint b37 = t + 35;
constraint b37 mod 37 = 0;

constraint b557 = t + 41;
constraint b557 mod 557 = 0;

constraint b29 = t + 43;
constraint b29 mod 29 = 0;

constraint b13 = t + 54;
constraint b13 mod 13 = 0;

constraint b17 = t + 58;
constraint b17 mod 17 = 0;

constraint b23 = t + 64;
constraint b23 mod 23 = 0;

constraint b419 = t + 72;
constraint b419 mod 419 = 0;

constraint b19 = t + 91;
constraint b19 mod 19 = 0;

solve minimize t;

output ["Star 2: ", show(t), "\n"];
