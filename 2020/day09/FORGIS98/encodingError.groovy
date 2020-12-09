#!/usr/bin/env groovy
println ""

def input = new File("input.txt") as String[];
input = input.collect {it.toLong()};
int preambulo = 25;

def isCorrect(ArrayList lista, Long valor, int preambulo){
  for(i in 0..preambulo){
    for(j in (i+1)..preambulo){
      if(lista[i] != lista[j] && lista[i] + lista[j] == valor){
        return true;
      }
    }
  }
  return false;
}

for (it=preambulo; it<input.size();){
  if(!isCorrect(input, input[it], preambulo)){
    println input[it];
    break;
  }
  input.removeAt(0)
}
