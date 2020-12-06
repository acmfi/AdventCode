#include <iostream>
#include <sstream>
#include <fstream>
#include <map>
#include <vector>
#include <regex>
#include <string>
#include <tuple>

size_t countInGroupAny(const std::string& group) {
	std::map<char,size_t> veces;
	for (auto c : group) {
		if (c == '\n') {
			continue;
		} else {
			veces[c]++;
		}
	}
	return(veces.size());
}

size_t countInGroupOnly(const std::string& group) {
	std::map<char, size_t> veces;
	size_t n_people = std::count(group.begin(), group.end(), '\n')+1;
	size_t onlyAll = 0;
	for (auto c : group) {
		if (c == '\n') {
			continue;
		}
		else {
			veces[c]++;
		}
	}
	for (auto const& [key, val] : veces) {
		if (val == n_people) {
			onlyAll++;
		}
	}
	return onlyAll;//podria returnear aqui el veces.size() y tneerlo todo en 1 funcion pero sudo xd
}

auto algorithm() {
	std::ifstream f(".\\6\\input.txt");
	std::stringstream buffer;
	buffer << f.rdbuf();
	std::string s = buffer.str();
	std::stringstream ss;
	ss << s << '\n';//Pa q no pase lo del dia 4
	s = ss.str();//gross!
	std::string delimiter = "\n\n";
	size_t pos = 0;
	std::string token;

//cosa q no es shit
	size_t any = 0,only = 0;
	while ((pos = s.find(delimiter)) != std::string::npos) {
		token = s.substr(0, pos);

		any += countInGroupAny(token);
		only += countInGroupOnly(token);

		s.erase(0, pos + delimiter.length());
	}
	return std::make_tuple(any,only);
}

int main() {
	auto[any,only] = algorithm();
	std::cout << "TOTALES Any: " << any << std::endl;
	std::cout << "TOTALES Only: " << only << std::endl;
	return getchar();
}










