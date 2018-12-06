let fs = require("fs");
const datos = fs.readFileSync("./input.txt").toString().split("\r\n");

let tablero = new Array(1000);
for (let i = 0; i < tablero.length; i++) {
    tablero[i] = new Array(1000);
    tablero[i].fill(0)
}
let parte1 = () => {
    let contador = 0;
    datos.forEach(linea =>{
        linea = linea.split(" ");
        let coordenada = linea[2].substr(0, linea[2].length-1).split(",");
        let cx = Number(coordenada[0]);
        let cy = Number(coordenada[1]);
        let dimension = linea[3].split("x");
        let dx = Number(dimension[0]);
        let dy = Number(dimension[1]);
        for(let i = cx; i< (dx+cx); i++){
            for(let j = cy; j< (dy+cy); j++){
                if(tablero[i][j] === 0)
                    tablero[i][j]++;
                else if(tablero[i][j] === 1){
                    contador++;
                    tablero[i][j]++;
                }
            }
        }
    });
    return contador;
};


let parte2 = () => {
    let ids = new Array(1346);
    for(let i = 0; i<ids.length; i++)
        ids[i] = i + 1;
    datos.forEach(linea =>{
        linea = linea.split(" ");
        let id = linea[0].substr(1, linea[0].length);
        let coordenada = linea[2].substr(0, linea[2].length-1).split(",");
        let cx = Number(coordenada[0]);
        let cy = Number(coordenada[1]);
        let dimension = linea[3].split("x");
        let dx = Number(dimension[0]);
        let dy = Number(dimension[1]);
        for(let i = cx; i< (dx+cx); i++){
            for(let j = cy; j< (dy+cy); j++){
                if(tablero[i][j] !== 0) {
                    ids[tablero[i][j] - 1] = 0;
                    ids[id-1] = 0;
                } else {
                    tablero[i][j] = id;
                }
            }
        }
    });
    let res = 0;
    ids.forEach(num =>{
        if(num !== 0) {
            res = num;
            return
        }
    });
    return res;
};

console.log(parte1());
console.log(parte2());