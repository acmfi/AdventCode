#include <fstream>
#include <sstream>
#include <iostream>
#include <vector>
#include <map>
#include <tuple>

std::map<std::string, std::vector<std::string>> bolsas;
std::map<std::string,std::vector<std::tuple<size_t,std::string>>> bolsasConCantidad;

void parseLine(std::string linea) {
	using namespace std;
	stringstream ss(linea);
	string aux;
	//key
	size_t pos = linea.find("bags");
	string key = linea.substr(0, pos - 1);//-1 pa quitar el espacio
	//values

	//primero quitamos hasta el contain
	pos = linea.find("contain");
	linea.erase(0,pos + 8);

	//dividimos de coma en coma
	vector<string> colores;
	while ((pos = linea.find(",")) != string::npos) {
		string color = linea.substr(0, pos);
		linea.erase(0, pos + 2);//hasta pos + la coma y el espacio
		colores.push_back(color);
	}
	//gestionamos el ultimo
	pos = linea.find(".");
	string color = linea.substr(0, pos);
	colores.push_back(color);
	
	//finally
	bolsas[key] = colores;

	/*vector<string> palabras;
	while (ss >> aux) {
		palabras.push_back(aux);
	}

	stringstream key;
	for (const auto& p : palabras) {
		key << p;
		if(p == "bags") break;
	}*/

}

//removes all metadata from the color info
void removeExtraInfo() {
	for (auto&[key, val] : bolsas) {
		bool toR = false;
		for (auto& v : val) {
			std::stringstream ss(v);
			size_t cuantas;
			ss >> cuantas;//unused 4 now

			size_t pos = v.find("bag");
			v.erase(pos-1, std::string::npos);//el -1 pal espacio
			pos = v.find(" ");
			v.erase(0,pos+1);

			if (v == "other") {
				toR = true;
				bolsasConCantidad[key].push_back(
					std::make_tuple(0,""));
			}
			else {
				bolsasConCantidad[key].push_back(
					std::make_tuple(cuantas,v));
			}

		}
		if (toR) {
			val.clear();
		}
	}
}

size_t profundiza(const std::string& currrent, const std::string& objetivo) {
	if (currrent == objetivo) {
		return 1;
	}
	else {
		size_t ret = 0;
		for (const auto& v : bolsas[currrent]) {
			ret += profundiza(v, objetivo);
		}
		return ret;
	}
}

size_t DFS(const std::string& objetivo) {
	size_t total = 0;
	for (auto&[key, val] : bolsas) {
		if (profundiza(key, objetivo)) {
			total++;
		}
		/*for (auto& v : val) {
			if (v == objetivo) {
				total++;
			}
			else {
				if (profundiza(v, objetivo)) {
					total++;
				}
			}
		}*/
	}
	return total;
}

size_t profundizaCuantas(const std::string& desde) {
	size_t ret = 1;

	for (const auto& [num,v] : bolsasConCantidad[desde]) {
		if (num == 0) {
			
		}else{
			ret += num * profundizaCuantas(v);
		}
		
	}
	return ret;
}
size_t DFSCuantas(const std::string& objetivo) {
	size_t total = 0;
	for (const auto&[num, v] : bolsasConCantidad[objetivo]) {
		total+= num*profundizaCuantas(v);
	}
	return total;
}

int day7main() {
	std::ifstream f("input.txt");
	std::string token;
	while (std::getline(f,token)) {
		parseLine(token);
	}
	removeExtraInfo();
	std::cout << DFS("shiny gold")-1<<std::endl;//-1 pa no contarse asi misma
	std::cout << DFSCuantas("shiny gold");
	return getchar();
}