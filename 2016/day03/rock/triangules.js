fs = require('fs');

function validTriangle(sides) {
    if (sides.length != 3) {
        console.log("wat: ", sides);
        return false
    }

    var x = sides[0],
        y = sides[1],
        z = sides[2];

    return x + y > z &&
           x + z > y &&
           y + z > x;
}

function analyze(data) {
    var validT = data.split("\n").map(x => x.trim().split(" ").filter(x => x != "").map(x => parseInt(x))).filter(validTriangle);
    return validT.length;
}


fs.readFile("input.txt", 'utf8', (err, data) => {
    if (err) {
        console.log("Error");
    } else {
        console.log(analyze(data));
    }
});
