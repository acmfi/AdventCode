#include <iostream>
#include <fstream>
#include <vector>
#include <string>
#include <algorithm>
#include <cmath>

struct Coord {
	size_t row;
	size_t col;
};


Coord convertToCoord(const std::string& s) {
	const size_t ROWS = 127;
	const size_t COLUMNS = 7;
	const size_t F_B_ELEMS = 7;
	const size_t L_R_ELEMS = 3;

	size_t elem = 0;
	size_t currentSum = ROWS/2;
	for (size_t i = 0; i < F_B_ELEMS; i++) {
		if (s[i] == 'B') {
			elem += currentSum + 1 ;
		}
		currentSum/=2;
	}

	size_t elemW = 0;
	size_t currentSumW = COLUMNS / 2;
	for (size_t i = F_B_ELEMS; i < (L_R_ELEMS + F_B_ELEMS); i++) {
		if (s[i] == 'R') {
			elemW += currentSumW + 1;
		}
		currentSumW /= 2;
	}
	Coord c;
	c.row = elem;
	c.col = elemW;
	//std::cout << "BoardingPass: " << s << " yields to " << elem << "," << elemW << std::endl;
	return c;
}

inline size_t getID(const Coord& c) {
	return(c.row*8+c.col);
}

int day5main(){
	std::ifstream input("./5/input.txt");

	std::string boardingPass;
	std::vector<size_t> seats;
	while (input >> boardingPass) {
		seats.push_back(getID(convertToCoord(boardingPass)));
		boardingPass = "";
	}
	std::cout << "\n\nMAX:  " << *std::max_element(seats.begin(),seats.end()) << std::endl;
	std::sort(seats.begin(), seats.end(), [](size_t a, size_t b) {
		return a > b;
	});
	for (int i = 1; i< seats.size(); i++) {
		if (seats[i] != seats[i - 1] - 1){
			std::cout << "MISSING: " << seats[i] +1 << std::endl;
		}
	}
	return getchar();
}