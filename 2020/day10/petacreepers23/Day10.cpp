#include <iostream>
#include <fstream>
#include <vector>
#include <thread>
#include <algorithm>
#include <future>
#include <cmath>
#include <map>

size_t combinaciones(std::vector<size_t> joltages) {	
	size_t jAct = 0;
	size_t saltos_3=0;
	size_t saltos_1=0;//Esto es una mierda xd
	for (const auto& j : joltages) {
		if(j-jAct==1)
			saltos_1++;
		if(j-jAct==3)
			saltos_3++;
		jAct = j;
	}
	saltos_3++;
	return saltos_1*saltos_3;
	
}
size_t fact(size_t n){

     return (n==0) || (n==1) ? 1 : n* fact(n-1);
}

//tarda demasiado
size_t hop(const size_t index, const std::vector<size_t>& joltages, size_t cont) {
	if (index == joltages.size() - 1) {
		return cont+1;
	}
	else {
		for (size_t i = 1;(index+i)<joltages.size() && i <= 3; i++)
		{
			if (joltages[index + i] - joltages[index]<=3) {
				cont = hop(index+i,joltages,cont);
			}
		}
		return cont;
	}
}

int day10main() {
	std::ifstream f("./10/input.txt");
	std::vector<size_t> joltages;
	size_t aux;

	while(f>>aux)
		joltages.push_back(aux);
	
	std::sort(joltages.begin(),joltages.end());

	std::cout << combinaciones(joltages) << std::endl;

	//|1|4|5|6|7|10|11|12|15...
	//1-1-1-2-4-4 -4 -8 -> Llegamos a 12 de 10 que vale 4 y de 11 que vale 4
	//		| -> Llegamos de 6 que vale 2, de 5 que vale 1 y de 4 que vale 1
	//		-> LLegamos de 5 y 4 que valen 1
	std::map<size_t,size_t> opcionesQueHaPodido;
	opcionesQueHaPodido[0] = 1;//Desde 0 solo hay 1 llegada
	for (size_t i = 0; i < joltages.size(); i++)
	{
		opcionesQueHaPodido[joltages[i]] =
			opcionesQueHaPodido[joltages[i]-1] +
			opcionesQueHaPodido[joltages[i]-2] +
			opcionesQueHaPodido[joltages[i]-3];//esto es una mierda
	}
	std::cout << opcionesQueHaPodido[joltages[joltages.size()-1]];



	/*Esto no funciona*/
	size_t jAct = 0;
	size_t cuantos = 1;
	for (size_t i = 0; i < joltages.size(); i++)
	{
		size_t saltos_posibles = 0;
		for (size_t j = i + 4; j > i; j--)
		{
			if (j >= joltages.size())
				break;
			if (std::abs((int)(joltages[j] - jAct)) <= 3)
				saltos_posibles++;
		}
		jAct = joltages[i];
		if(saltos_posibles>1)
			cuantos += saltos_posibles;
		else
			cuantos+= cuantos;
	}

	return getchar();
}