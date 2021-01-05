#include "StringUtils.hpp"

#include <fstream>
#include <string>
#include <iostream>
#include <map>

typedef unsigned int ui;//4 bytes unsigned integer

inline size_t maskear(const std::map<ui, ui>& mask, size_t num) {
	for (auto [pos, val] : mask) {
		if (val == 1) {
			num |= ((size_t)1 << pos);
		}
		else {
		//para la parte 1 descomenta esto xd
			//num &= ~((size_t)1 << pos);
		}
	}
	return num;
}

std::vector<size_t> maskearAddresses(const std::map<ui, ui>& mask, size_t addr) {
	std::vector<size_t> addresses;
	addr = maskear(mask,addr);
	addresses.push_back(addr);
	for (size_t i = 0; i < 36; i++)
	{
		if (mask.find(i) == mask.end()) {
			auto actual_addr = addresses;
			for (auto a : actual_addr) {
				size_t aux = a;
				aux |= ((size_t)1 << i);
				addresses.push_back(aux);
				aux &= ~((size_t)1 << i);
				addresses.push_back(aux);
			}
			
		}
		
	}
	return addresses;
}

int main() {
	std::ifstream f("input.txt");
	std::string aux;
	
	std::map<size_t, size_t> memoria;//Si, un map en vez de vector pa no andar buscando el maximo
	std::map<ui, ui> mask;

	while (std::getline(f,aux)) {
		auto partes = P23::split(aux," = ");
		if (partes[0] == "mask") {
			mask.clear();
			std::reverse(partes[1].begin(),partes[1].end());
			for (ui i = 0; i < partes[1].size(); i++) {
				if(partes[1][i]!='X')
					mask[i] = (partes[1][i]-'0');
			}
		}
		else {
			ui address;
			size_t value = std::stoi(partes[1]);
			sscanf_s(partes[0].c_str(),"mem[%d]",&address);
			auto v = maskearAddresses(mask, address);
			for (auto a : v) {
				memoria[a] = value;
			}
		}
	}

	size_t res = 0;
	for (auto [k, v] : memoria) {
		res+=v;
	}
	std::cout << "RES: " << res;
	return getchar();//1087023102109,//1087023101698
}