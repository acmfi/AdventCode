#!/usr/bin/env groovy
println ""

def input = new File(args[0]) as String[];
input = input.collect {it.toInteger()};
input = input.sort();
input.add(0, 0);

def difUno = 0
def difTres = 1

(input.size()-1).times{
  if((input[it]+1) == input[it+1])
    difUno += 1
  else if((input[it]+3) == input[it+1])
    difTres += 1
}

println (difUno * difTres)

