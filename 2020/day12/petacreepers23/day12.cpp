#include <iostream>
#include <fstream>
#include <string>
#include <vector>

inline void tilt(const int grados,const int parametro, int& norte, int& este) {
	//Calcula el movimiento
	switch (grados) {
	case 0:
		norte += parametro;
		break;
	case 90:
		este -= parametro;
		break;
	case 180:
		norte -= parametro;
		break;
	case 270:
		este += parametro;
		break;
	}
}

void parte1() {
	std::fstream f("./12/input.txt");

	int norte = 0, este = 0, grados = 270;//facing east

	std::string aux;
	while (f >> aux) {
		int parametro = std::stoi(aux.substr(1, std::string::npos));
		//Calcula la direccion
		switch (aux[0]) {
		case 'N':
			tilt(0, parametro, norte, este);
			break;
		case 'S':
			tilt(180, parametro, norte, este);
			break;
		case 'E':
			tilt(270, parametro, norte, este);
			break;
		case 'W':
			tilt(90, parametro, norte, este);
			break;
		case 'L':
			parametro %= 360;
			grados += parametro;
			if (grados >= 360) {
				grados = grados - 360;
			}
			break;
		case 'R':
			parametro %= 360;
			grados -= parametro;
			if (grados < 0) {
				grados = 360 + grados;
			}
			break;
		case 'F':
			tilt(grados, parametro, norte, este);
			break;
		default:
			break;
		}

	}
	std::cout << std::abs(norte) + std::abs(este);
}

void parte2() {
	std::fstream f("./12/input.txt");

	int norte = 0, este = 0;//,grados = 270;//facing east
	int wa_norte = 1, wa_este = 10;

	//auxiliares
	int vOeste, vNorte, vEste, vSur;
	std::string aux;
	while (f >> aux) {
		int parametro = std::stoi(aux.substr(1, std::string::npos));
		//Calcula la direccion
		switch (aux[0]) {
		case 'N':
			wa_norte += parametro;
			break;
		case 'S':
			wa_norte -= parametro;
			break;
		case 'E':
			wa_este += parametro;
			break;
		case 'W':
			wa_este -= parametro;
			break;
		case 'L':
			parametro %= 360;
			switch (parametro) {
			case 0:
				break;
			case 90:
				vOeste = wa_norte;
				vNorte = wa_este;
				wa_este = -vOeste;
				wa_norte = vNorte;
				break;
			case 180:
				wa_este = -wa_este;
				wa_norte = -wa_norte;
				break;
			case 270:
				vEste = wa_norte;
				vSur = wa_este;
				wa_este = vEste;
				wa_norte = -vSur;
				break;
			}
			break;
		case 'R':
			parametro %= 360;
			parametro %= 360;
			switch (parametro) {
			case 0:
				break;
			case 90:
				vEste = wa_norte;
				vSur = wa_este;
				wa_este = vEste;
				wa_norte = -vSur;
				break;
			case 180:
				wa_este = -wa_este;
				wa_norte = -wa_norte;
				break;
			case 270:
				vOeste = wa_norte;
				vNorte = wa_este;
				wa_este = -vOeste;
				wa_norte = vNorte;
				break;
			}
			break;
		case 'F':
			norte += parametro * wa_norte;
			este += parametro * wa_este;
			break;
		default:
			break;
		}

	}
	std::cout << std::abs(norte) + std::abs(este);
}

int day12main() {
	parte1();
	parte2();
	return getchar();
}