let fs = require("fs");
let datos = fs.readFileSync("./input.txt").toString().split("\r\n");

let parte1 = () => {
    datos.sort();
    let guardias = [];
    let guardiasHoras = [];
    let guardiaActual = "";
    let horaDormido = "";
    datos.forEach(linea =>{
        linea = linea.split(" ");
        if(linea[2] === "Guard" && !guardias.includes(linea[3])){
            guardias.push(linea[3]);
            guardiasHoras.push(0);
            guardiaActual = linea[3];
        } else if(linea[2] === "Guard")
            guardiaActual = linea[3];
        else if(linea[2] === "falls") {
            horaDormido = linea[1].substr(0, linea[1].length - 1);
        }else{
            let horaDespierto = linea[1].substr(0, linea[1].length - 1);
            guardiasHoras[guardias.indexOf(guardiaActual)] += calcularTiempoDormido(horaDormido, horaDespierto)
        }
    });
    let id = guardias[guardiasHoras.indexOf(Math.max.apply(null, guardiasHoras))];
    return minutoMaxRepetido(id)*id.substr(1,id.length);
};

function calcularTiempoDormido(duerme, despierta){
    let arrDuerme = duerme.split(":");
    let arrDespierta = despierta.split(":");
    if(arrDuerme[0] === "23")
        return  60 - Number(arrDuerme[1]) + Number(arrDespierta[1]);
    return Number(arrDespierta[1]) - Number(arrDuerme[1]);
}

function minutoMaxRepetido(persona){
    let horas = new Array(60).fill(0);
    datos.sort();
    let anadir = false;
    let horaDormido = [];
    datos.forEach(linea => {
        linea = linea.split(" ");
        if(linea[2] === "Guard" && linea.includes(persona)){anadir = true;}
        else if(linea[2] === "Guard" && !linea.includes(persona)) {anadir = false;}
        if(anadir){
            let i = 0;
            if(linea[2] === "falls") {
                horaDormido = linea[1].substr(0, linea[1].length - 1).split(":");
            }else{
                let horaDespierto = linea[1].substr(0, linea[1].length - 1).split(":");
                for(let j = horaDormido[1]; j<horaDespierto[1]; j++)
                    horas[j]++;
            }
        }
    });
    return horas.indexOf(Math.max.apply(null, horas));
}

let parte2 = () => {
    datos.sort();
    let guardiasId = [];
    let guardiasHoras = [];
    let guardiaActual = "";
    let horaDormido = [];
    datos.forEach(linea => {
        linea = linea.split(" ");
        if(linea[2] === "Guard" && !guardiasId.includes(linea[3])){
            guardiasId.push(linea[3]);
            guardiasHoras.push(new Array(60).fill(0));
            guardiaActual = linea[3];
        } else if(linea[2] === "Guard") {
            guardiaActual = linea[3];
        }
        else if(linea[2] === "falls") {
            horaDormido = linea[1].substr(0, linea[1].length - 1).split(":");
        }else{
            let horaDespierto = linea[1].substr(0, linea[1].length - 1).split(":");
            for(let j = horaDormido[1]; j<horaDespierto[1]; j++)
                guardiasHoras[guardiasId.indexOf(guardiaActual)][j]++;
        }
    });
    let contMin = 0;
    let minMayor = 0;
    let posId = 0;
    for(let i = 0; i<guardiasHoras.length; i++){
        let cont = Math.max.apply(null, guardiasHoras[i]);
        if(contMin < cont){
            contMin = cont;
            minMayor = guardiasHoras[i].indexOf(cont);
            posId = i;
        }
    }
    return minMayor*guardiasId[posId].substr(1)
};


console.log(parte1());
console.log(parte2());