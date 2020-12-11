#include <algorithm>
#include <iostream>
#include <sstream>
#include <fstream>
#include <numeric>
#include <string>
#include <vector>
#include <tuple>
#include <map>

bool algoritmoDia1(const std::vector<size_t>& valores,size_t objetivo) {
	for (size_t i = 0; i < valores.size(); i++) {
		for (size_t j = i + 1; j < valores.size(); j++) {
			if (valores[j] + valores[i] == objetivo) {
				return true;
			}
		}
	}
	return false;
}

size_t findOutlier(const std::vector<size_t>& numeros) {
	for (size_t i = 0; i + 25 < numeros.size(); i++)
	{
		std::vector<size_t>  subvector(numeros.begin() + i, numeros.begin() + i + 25);
		if (!algoritmoDia1(subvector, numeros[i + 25])) {
			return  numeros[i + 25];
		}
	}
	return 0;
}

size_t parseAndRun() {
	std::ifstream f(".\\9\\input.txt");
	std::vector<size_t> numeros;
	size_t aux;
	while (f >> aux) {
		numeros.push_back(aux);
	}
	size_t outlier = findOutlier(numeros);
	std::cout << "ESTRELLA 1: " << outlier << std::endl << std::endl;
	
	for (size_t i = 0; i < numeros.size(); i++)
	{
		size_t suma = 0;
		for (size_t inCont = i; inCont < numeros.size(); inCont++)
		{
			suma += numeros[inCont];
			if (suma == outlier && inCont!=i) {
				return (*std::min_element(numeros.begin() + i, numeros.begin() + inCont)) + 
						(*std::max_element(numeros.begin() + i, numeros.begin() + inCont));
			}
			if (suma > outlier) {
				break;
			}
		}
	}
	return 0;
}

int day9main() {
	size_t notSum = parseAndRun();
	std::cout << "ESTRELLA 2: " << notSum << std::endl;
	return getchar();
}