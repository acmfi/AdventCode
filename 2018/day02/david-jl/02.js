let fs = require("fs");
const datos = fs.readFileSync("./input.txt").toString().split("\n");

let parte1 = () =>{
    let dobles = 0, triples = 0;
    datos.forEach(linea => {
        let letras = new Array(300);
        letras.fill(0);
        for(let i = 0; i<linea.length; i++)
            letras[linea.charCodeAt(i)] += 1;
        let doblesEnc = false, triplesEnc = false;
        for(let i = 0; i<letras.length && (!doblesEnc || !triplesEnc); i++){
            if(letras[i] === 2 && !doblesEnc){
                dobles++;
                doblesEnc = true;
            }
            else if(letras[i] === 3 && !triplesEnc){
                triples++;
                triplesEnc = true;
            }
        }
    });
    return dobles*triples;
};

let parte2 = () => {
    let sol = ["", 0];
    datos.forEach(linea1 =>{
        datos.forEach(linea2 => {
            if(linea1 !== linea2){
                let contador = 0, i; fallo = 0;
                for(i = 0; i<linea1.length; i++){
                    if(linea1.charAt(i) !== linea2.charAt(i)) {
                        contador++;
                        fallo = i;
                    }
                }
                if(contador === 1) {
                    sol = [linea1, fallo];
                    return;
                }
            }
        });
        if(sol[0] !== "")
            return;
    })
    return sol[0].substr(0,sol[1]) + sol[0].substr(sol[1] + 1, sol[0].length)
};


console.log(parte1());
console.log(parte2());