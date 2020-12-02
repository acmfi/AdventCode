#include <iostream>
#include <string>
#include <fstream>
#include <vector>
#include <regex>

struct PasswdData
{
	int min;
	int max;
	char ch;
	std::string passwd;
};

void algoritmo1() {
	std::ifstream input("input1.txt");
	
	std::string line;
	std::regex regex("^([0-9]+)-([0-9]+)\\s([a-z]):\\s(\\w+)$");
	std::smatch res;

	int validas = 0;
    while (std::getline(input, line))
    {
		std::cout << line << std::endl;

        std::regex_match(line,res,regex);
		PasswdData aux;
		aux.min = std::stoi(res[1]);
		std::cout << "Min: " << aux.min;
		aux.max = std::stoi(res[2]);
		std::cout << " Max: " << aux.max;
		aux.ch = std::string(res[3])[0];
		std::cout << " ch: " << aux.ch;
		aux.passwd = res[4];
		std::cout << " Passwd: " << aux.passwd;

		int veces = 0;
		for (auto ch : aux.passwd) {
			if (ch == aux.ch) {
				veces++;
			}
		}
		std::cout << "\tVeces: " << veces << std::endl;
		if (veces >= aux.min && veces <= aux.max) {
			std::cout << "Valida\n";
			validas++;
		}
		std::cout << std::endl;
	}
	std::cout << "TOTAL:  ---------------------" << std::endl << validas;

}

void algoritmo2() {
	std::ifstream input("input2.txt");
	
	std::string line;
	std::regex regex("^([0-9]+)-([0-9]+)\\s([a-z]):\\s(\\w+)$");
	std::smatch res;

	int validas = 0;
    while (std::getline(input, line))
    {
		std::cout << line << std::endl;

        std::regex_match(line,res,regex);
		PasswdData aux;
		aux.min = std::stoi(res[1]);
		std::cout << "Min: " << aux.min;
		aux.max = std::stoi(res[2]);
		std::cout << " Max: " << aux.max;
		aux.ch = std::string(res[3])[0];
		std::cout << " ch: " << aux.ch;
		aux.passwd = res[4];
		std::cout << " Passwd: " << aux.passwd;


		if (aux.passwd[aux.min - 1] == aux.ch && aux.passwd[aux.max - 1] == aux.ch) {
			std::cout << std::endl << "Invalida, esta la c en min y max." << std::endl;
			continue;
		}
		if (aux.passwd[aux.min - 1] == aux.ch || aux.passwd[aux.max - 1] == aux.ch) {
			std::cout << std::endl << "Valida, esta la c no esta en min y max a la vez." << std::endl;
			validas++;
			continue;
		}
	


		std::cout << std::endl;
	}
	std::cout << "TOTAL:  ---------------------" << std::endl << validas;

}

int main() {
	algoritmo2();
	return getchar();
}