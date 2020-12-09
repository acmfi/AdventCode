#!/usr/bin/env groovy
println ""

def input = new File(args[0]) as String[];
input = input.collect {it.toLong()};
def starTwoInput = input.clone()
int preambulo = 25;

// PRIMERA ESTRELLA //
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

long _BREAK = 0;
for (it=preambulo; it<input.size();){
  if(!isCorrect(input, input[it], preambulo)){
    _BREAK = input[it];
    break;
  }
  input.removeAt(0)
}

// SEGUNDA ESTRELLA //
def aSumar = [];
for (it=0; it<starTwoInput.size();){
  if(aSumar.sum() == null || aSumar.sum() < _BREAK){
    aSumar.add(starTwoInput[it].toLong());
    it++;
  }
  else if(aSumar.sum() == _BREAK){
    println aSumar.min() + aSumar.max()
    break
  }
  else
    aSumar.removeAt(0)
}
