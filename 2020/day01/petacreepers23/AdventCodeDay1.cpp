#include <iostream>
#include <fstream>
#include <vector>

//Encuentra 2 numeros en el array cuya suma sea 2020
void algoritmo1(const std::vector<int>& valores) {
	for (size_t i = 0; i < valores.size(); i++) {
		for (size_t j = i + 1; j < valores.size(); j++) {
			if (valores[j] + valores[i] == 2020) {
				std::cout << valores[j] << "," << valores[i] << ": " <<
					valores[j] * valores[i] << std::endl;
			}
		}
	}
}

//Encuentra 3 numeros en el array cuya suma sea 2020
void algoritmo2(const std::vector<int>& valores) {
	for (size_t i = 0; i < valores.size(); i++) {
		for (size_t j = i + 1; j < valores.size(); j++) {
			for (size_t k = j + 1; k < valores.size(); k++) {
				if (valores[k] + valores[j] + valores[i] == 2020) {
					std::cout << valores[j] << "," << valores[i] << "," << valores[k]  <<
					": " << valores[j] * valores[i] * valores[k] << std::endl;
				}
			}
		}
	}
}

int main() {
	std::ifstream input("./1/input2.txt");
	
	std::vector<int> valores;
	int aux;

	while (input >> aux) {
		valores.push_back(aux);
	}

	algoritmo2(valores);

	return getchar();
}