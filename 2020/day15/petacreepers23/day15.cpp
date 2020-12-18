#include <iostream>
#include <vector>
#include <algorithm>
#include <unordered_map>
using Index = std::size_t;
constexpr Index TARGET_2 = 30000000;

int main() {
    std::vector<Index> input = { 1,20,8,12,0,14 };
    std::cout << play_until_n_v2(input, TARGET_2);
}

int day15Main() {
	std::vector<size_t> numeros = { 0,3,6 };
	for (size_t i = numeros.size(); i < 30000000; i++)//empezó a las 8:50am, acaba a las 21:50pm 
	{
		size_t aConsiderar = numeros[i-1];
		if (std::find(numeros.begin(), numeros.end()-1, aConsiderar) == numeros.end()-1) {
			numeros.push_back(0);
		}
		else {
			auto rIter = std::find(numeros.rbegin(),numeros.rend(), aConsiderar);

			size_t primAparicion = std::distance(numeros.begin(), rIter.base());

			size_t secAparicion = std::distance(numeros.begin(), std::find(++rIter, numeros.rend(), aConsiderar).base());

			numeros.push_back(primAparicion- secAparicion);
		}
	}
	std::cout << numeros[numeros.size()-1];
	return getchar();//63644
}