#include <iostream>
#include <string>
#include <fstream>
#include <vector>
#include <regex>

size_t iterarMatriz(const std::vector<std::string>& matriz, size_t n_dcha, size_t n_abajo) {
	size_t dcha = 0;
	size_t totales = 0;
	for (size_t cols = 0; cols < matriz.size(); cols+= n_abajo)
	{		
		if (matriz[cols][dcha] == '#') {
			totales++;
		}
		dcha+=n_dcha;
		dcha%=matriz[cols].length();
	}
	return totales;
}


void algoritmo1(std::string filename) {
	std::ifstream input(filename);
	
	std::vector<std::string> lineas;
	std::string line;
	int validas = 0;
    while (std::getline(input, line))
    {
		lineas.push_back(line);
	}
	std::cout << "TOTAL:  ---------------------" << std::endl << "\t\t" <<	iterarMatriz(lineas,3,1);

}

void algoritmo2(std::string filename) {
	std::ifstream input(filename);
	
	std::vector<std::string> lineas;
	std::string line;
	int validas = 0;
    while (std::getline(input, line))
    {
		lineas.push_back(line);
	}

	size_t trees_slopes[5];
	trees_slopes[0] = iterarMatriz(lineas,1,1);
	trees_slopes[1] = iterarMatriz(lineas,3,1);
	trees_slopes[2] = iterarMatriz(lineas,5,1);
	trees_slopes[3] = iterarMatriz(lineas,7,1);
	trees_slopes[4] = iterarMatriz(lineas,1,2);

	std::cout << "TOTAL: " << trees_slopes[0]*trees_slopes[1]*trees_slopes[2]*trees_slopes[3]*trees_slopes[4];
}

int main() {
	algoritmo2("input1.txt");
	return getchar();
}