let fs = require("fs");
const datos = fs.readFileSync("./input.txt").toString().split("\n");

let parte1 = () =>{
    let res = 0;
    datos.forEach(num => res += Number(num));
    return res;
};

let prueba = [+7, +7, -2, -7, -4];

let parte2 = () => {
    let memoria = [];
    let res = 0, i = 0;
    let long = datos.length;
    while(true){
        res += Number(datos[i%long]);
        if(memoria.includes(res)) {return res;}
        else {memoria.push(res)}
        i++;
    }
};

console.log(parte1());
console.log(parte2());

