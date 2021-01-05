#include <iostream>
#include <fstream>
#include <string>
#include <vector>
#include <tuple>
#include <map>

#include "StringUtils.hpp"

template<typename _Tk, typename Tv> bool isInValues(const std::map<_Tk, Tv>& map, const Tv& v) {
	for (const auto&[key, val] : map) {
		if (val == v)
			return true;
	}
	return false;
}

struct RunOnce {
	template<typename T> RunOnce(T&& f) { f(); }
};

int main() {
	std::fstream cc("Contitions.txt");
	std::vector<std::pair<size_t,size_t>> condiciones;
	std::string caux;
	while (std::getline(cc, caux)) {
		auto v1 = P23::split(caux,": ");
		auto v2 = P23::split(v1[1], " or ");
		auto v2_1 = P23::split(v2[0], "-");
		std::pair<size_t, size_t> p1 = { std::stoi(v2_1[0]),std::stoi(v2_1[1]) };
		auto v2_2 = P23::split(v2[1], "-");
		std::pair<size_t, size_t> p2 = { std::stoi(v2_2[0]),std::stoi(v2_2[1]) };
		condiciones.push_back(p1);
		condiciones.push_back(p2);
	}
	cc.close();

	size_t suma = 0;
	std::fstream f("Nearby.csv");

	std::vector<std::vector<std::string>> tickets;
	std::vector<std::string> MI_TICKET;

	std::string aux;
	while (std::getline(f, aux)) {
		auto v = P23::split(aux,",");
		static RunOnce guardar_mi_ticket([&]() {MI_TICKET = v; });

		bool t_invalid = false;
		for (auto e : v)
		{
			size_t curr = std::stoi(e);
			bool invalid = false;
			for (auto c : condiciones)
			{
				if(curr < c.first || curr > c.second)
					invalid = true;
				else{
					invalid = false;
					break;
				}
			}
			if (invalid) {
				suma +=curr;
				t_invalid = true;
			}	
		}
		if (!t_invalid) {
			tickets.push_back(v);
		}
	}
	f.close();
	std::cout << suma << std::endl << std::endl << std::endl << std::endl << std::endl;

	std::map<size_t,std::vector<size_t>> posibles_pa_cada_posicion;//guarda los indices de las condiciones posibles
	for (size_t i = 0; i < condiciones.size(); i+=2){
		for (const auto& ticket : tickets) {
			size_t posicion = 0;
			for (auto elemento : ticket){
				size_t e = std::stoi(elemento);
				if ((e >= condiciones[i].first && e <= condiciones[i].second)||
					(e >= condiciones[i+1].first && e <= condiciones[i+1].second)) {
					posibles_pa_cada_posicion[i].push_back(posicion);
				}
				posicion++;
			}
		}
	}
	std::vector<std::map<size_t, size_t>> lo_mismo_que_pqhcqe_pero_solo_con_las_veces_de_cada_num;
	for (auto [id_condicion, posiciones_que_han_corroborado_que_estan] : posibles_pa_cada_posicion) {
		std::map<size_t,size_t> veces;
		for (auto n : posiciones_que_han_corroborado_que_estan)
		{
			veces[n]++;
		}
		lo_mismo_que_pqhcqe_pero_solo_con_las_veces_de_cada_num.push_back(veces);
	}

	std::vector<size_t> cantidad_sitios_posibles;
	for (auto m : lo_mismo_que_pqhcqe_pero_solo_con_las_veces_de_cada_num) {
		size_t posibilidades = 0;
		for (auto [k, v] : m) {
			if(v!=190)
				posibilidades++;
		}
		cantidad_sitios_posibles.push_back(posibilidades);
	}

	std::map<size_t,size_t> condicion_corresponde_con_posicion;
	for (size_t i = 1; i < 20; i++)
	{
		size_t primer_Indice = std::distance(cantidad_sitios_posibles.begin(),std::find(cantidad_sitios_posibles.begin(), cantidad_sitios_posibles.end(), i));//condicion
		auto valores = lo_mismo_que_pqhcqe_pero_solo_con_las_veces_de_cada_num[primer_Indice];

		size_t turno = 999;
		for (auto [k, v] : valores) {
			if (v != 190 && !isInValues(condicion_corresponde_con_posicion,k)) {
				turno = k;
				break;
			}
		}
		condicion_corresponde_con_posicion[primer_Indice] = turno;//el num que en valores no sea 190 y no esté ya en el map
		std::cout << primer_Indice << " " << turno << std::endl;
	}
	std::cout << std::endl << std::endl << std::endl;
	size_t reslut_2 = 1;
	for (size_t i = 0; i <= 5; i++)
	{
		reslut_2 *= std::stoi(MI_TICKET[condicion_corresponde_con_posicion[i]]);
	}
	std::cout << reslut_2;
	return getchar();
}

