#include <vector>
#include <fstream>
#include <iostream>
#include <unordered_map>

inline bool cambia1(const size_t& x, const size_t& y, const std::vector<std::string>& matriz) {
//Ya se que hay comprobaciones innecesarias, no me lo repitas
	size_t contador = 0;
	/*arriba*/
	if ((x - 1) < matriz[0].size() && (y - 1) < matriz.size() &&
	matriz[y - 1][x - 1]=='#')
		contador++;
	if ((x) < matriz[0].size() && (y - 1) < matriz.size() &&
	matriz[y - 1][x] == '#')
		contador++;
	if ((x + 1) < matriz[0].size() && (y - 1) < matriz.size() &&
	matriz[y - 1][x + 1] == '#')
		contador++;
	/*en fila*/
	if ((x - 1) < matriz[0].size() && (y) < matriz.size() &&
	matriz[y][x - 1] == '#')
		contador++;
	if ((x + 1) < matriz[0].size() && (y) < matriz.size() &&
	matriz[y][x + 1] == '#')
		contador++;
	/*debajo*/
	if ((x - 1) < matriz[0].size() && (y + 1) < matriz.size() &&
	matriz[y + 1][x - 1] == '#')
		contador++;
	if ((x) < matriz[0].size() && (y + 1) < matriz.size() &&
	matriz[y + 1][x] == '#')
		contador++;
	if ((x + 1) < matriz[0].size() && (y + 1) < matriz.size() &&
	matriz[y + 1][x + 1] == '#')
		contador++;

	if (matriz[y][x] == 'L' && contador==0) {
		return true;
	}
	if (matriz[y][x] == '#' && contador >= 4) {
		return true;
	}
	return false;
}

inline bool cambia2(const size_t& i, const size_t& j, const std::vector<std::string>& matriz) {
	//Ya se que hay comprobaciones innecesarias, no me lo repitas
	int contador = 0,x,y;
	/*diagonal pa arriba*/
	x = i; y = j;
	x--; y--;
	while (x < matriz[0].size() && y < matriz.size() && x >= 0 && y >= 0) {
		if (matriz[y][x] == '#') {
			contador++;
			break;
		}
		if (matriz[y][x] == 'L') {
			break;
		}
		x--; y--;
	}
	/*pa arriba*/
	x = i; y = j;
	y--;
	while (x < matriz[0].size() && y < matriz.size() && x >= 0 && y >= 0) {
		if (matriz[y][x] == '#') {
			contador++;
			break;
		}
		if (matriz[y][x] == 'L') {
			break;
		}
		y--;
	}
	/*la otra diagonal pa arriba*/
	x = i; y = j;
	x++; y--;
	while (x < matriz[0].size() && y < matriz.size() && x >= 0 && y >= 0) {
		if (matriz[y][x] == '#') {
			contador++;
			break;
		}
		if (matriz[y][x] == 'L') {
			break;
		}
		x++; y--;
	}
	/*pa izquierda*/
	x = i; y = j;
	x--;
	while (x < matriz[0].size() && y < matriz.size() && x >= 0 && y >= 0) {
		if (matriz[y][x] == '#') {
			contador++;
			break;
		}
		if (matriz[y][x] == 'L') {
			break;
		}
		x--;
	}
	/*pa derecha*/
	x = i; y = j;
	x++;
	while (x < matriz[0].size() && y < matriz.size() && x >= 0 && y >= 0) {
		if (matriz[y][x] == '#') {
			contador++;
			break;
		}
		if (matriz[y][x] == 'L') {
			break;
		}
		x++;
	}
	/*diagonal pa abajo*/
	x = i; y = j;
	x++; y++;
	while (x < matriz[0].size() && y < matriz.size() && x >= 0 && y >= 0) {
		if (matriz[y][x] == '#') {
			contador++;
			break;
		}
		if (matriz[y][x] == 'L') {
			break;
		}
		x++; y++;
	}
	/*pa abajo*/
	x = i; y = j;
	y++;
	while (x < matriz[0].size() && y < matriz.size() && x >= 0 && y >= 0) {
		if (matriz[y][x] == '#') {
			contador++;
			break;
		}
		if (matriz[y][x] == 'L') {
			break;
		}
		y++;
	}
	/*la otra diagonal pa arriba*/
	x = i; y = j;
	x--; y++;
	while (x < matriz[0].size() && y < matriz.size() && x >= 0 && y >= 0) {
		if (matriz[y][x] == '#') {
			contador++;
			break;
		}
		if (matriz[y][x] == 'L') {
			break;
		}
		x--; y++;
	}




	/*esto es casi igual*/
	if (matriz[j][i] == 'L' && contador == 0) {
		return true;
	}
	if (matriz[j][i] == '#' && contador >= 5) {
		return true;
	}
	return false;
}

int main() {
	std::ifstream f("./11/input.txt");

	std::vector<std::string> asientos;
	std::string aux;
	while (f>>aux) {
		asientos.push_back(aux);
	}

	std::unordered_map<size_t,bool> mascara;

	size_t nCambios;

	do {
		nCambios = 0;
		//Vemos que cambios hay
		for (size_t y = 0; y < asientos.size(); y++) {
			for (size_t x = 0; x < asientos[0].size(); x++) {
				bool hayCambio = cambia2(x,y,asientos);
				mascara[y * asientos[0].size() + x] = hayCambio;
				nCambios += hayCambio;
			}
		}
		//los cambiamos
		for (size_t y = 0; y < asientos.size(); y++) {
			for (size_t x = 0; x < asientos[0].size(); x++) {
				if (mascara[y * asientos[0].size() + x]) {
					if (asientos[y][x] == '#') {
						asientos[y][x] = 'L';
					}else if (asientos[y][x] == 'L') {
						asientos[y][x] = '#';
					}
				}
			}
		}
	}while(nCambios>0);
	
	size_t resultado = 0;
	for (size_t y = 0; y < asientos.size(); y++) {
		for (size_t x = 0; x < asientos[0].size(); x++) {
			if (asientos[y][x] == '#') {
				resultado++;
			}
		}
	}
	std::cout << "RESULTADO!: "<< resultado << std::endl;
	return getchar();
}
