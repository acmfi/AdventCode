#include <iostream>
#include <fstream>
#include <vector>
#include <tuple>

#include "StringUtils.hpp"

template<typename T> T mod_pythonic(T a, T b) {
	return (b+((a)%b))%b;
}

int obtenerInverso(int a, int m)
{
	int c1 = 1;
	int c2 = -(m / a); //coeficiente de a y b respectivamente
	int t1 = 0;
	int t2 = 1; //coeficientes penultima corrida
	int r = m % a; //residuo, asignamos 1 como condicion de entrada 
	int x = a, y = r, c;
	while (r != 0)
	{
		c = x / y;//cociente
		r = x % y;//residuo
		//guardamos valores temporales de los coeficientes
		//multiplicamos los coeficiente por -1*cociente de la division
		c1 *= -c;
		c2 *= -c;
		//sumamos la corrida anterior
		c1 += t1;
		c2 += t2;
		//actualizamos corrida anterior
		t1 = -(c1 - t1) / c;
		t2 = -(c2 - t2) / c;
		x = y;
		y = r;
	}
	return t2;
	
}


int main() {
	std::ifstream f("input.txt");
	size_t timestamp;
	std::string buses;
	f>>timestamp;
	f>>buses;
	auto lineas = P23::split(buses,",");

	int espera = 999999;
	int minimoBus = 0;
	for (auto& s : lineas) {
		if(s=="x")
			continue;
		int frecuenciaDelBus = std::stoi(s);
		int haceCuanto = timestamp % frecuenciaDelBus;//hace 1 min
		int tiempoSiguiente = timestamp- haceCuanto+ frecuenciaDelBus;
		int tiempoEspera = tiempoSiguiente- timestamp;
		if (tiempoEspera < espera) {
			espera = tiempoEspera;
			minimoBus = frecuenciaDelBus;
		}

	}

	std::cout << espera * minimoBus<<std::endl;

	///Parte2

	size_t offset = 1;
	size_t diferencia = 0;
	std::vector<std::tuple<size_t,size_t>>ecuaciones;//x congruente con tuple(0) mod tuple(1)
	for (size_t i = 0; i < lineas.size(); i++)
	{
		if(lineas[i]=="x")
			continue;
		//ecuaciones.push_back(std::make_tuple((std::stoi(lineas[i])-i)% std::stoi(lineas[i]),
		ecuaciones.push_back(std::make_tuple((std::stoi(lineas[i]) - i), std::stoi(lineas[i])));
	}
	//teorema chno del resto
	//ecuaciones.clear();
	//ecuaciones = {std::make_tuple(2,3),std::make_tuple(3,5),std::make_tuple(2,7) };

	//1: calculamos p
	size_t p = 1;
	std::vector<std::tuple<size_t, size_t>>vectorDeQs;
	for (auto [v, mod] : ecuaciones)
		p*=mod;
	//2: calculamos Qi
	for (auto [v, mod] : ecuaciones)
		vectorDeQs.push_back(std::make_tuple(p/mod,mod));
	//3: calculamos la solucion modulo p
	size_t solucion = 0;
	for (size_t i = 0; i < ecuaciones.size(); i++){
		auto [v, mod] = ecuaciones[i];
		auto [q, mod2] = vectorDeQs[i];

		int Qi = obtenerInverso(q%mod, mod);
		Qi = mod_pythonic(Qi,(int)mod);
		solucion += v * Qi * q;
	}
		

	std::cout << solucion%p;//(+k*p)

	return getchar();
}