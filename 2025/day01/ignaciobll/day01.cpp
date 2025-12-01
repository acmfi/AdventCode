#include <iostream>
#include <string>
#include <vector>

int star1(std::vector<int> xs) {
    int zeros = 0;
    int position = 50;
    for (int distance : xs) {
        position += (distance % 100);
        position = (position + 100) % 100;
        if (position == 0) { ++zeros; }
    }
    return zeros;
}

int star2(std::vector<int> xs) {
    int zeros = 0;
    int position = 50;
    for (int distance : xs) {
        int sign = (distance > 0) ? 1 : ((distance < 0) ? -1 : 0);
        zeros += std::abs(distance) / 100;

        distance = sign * (std::abs(distance) % 100);

        int starting_position = position;
        position += (distance % 100);
        if (starting_position != 0 && (position < 0 || position > 100)) { ++zeros; }
        position = (position + 100) % 100;
        if (position == 0) { ++zeros; }
    }
    return zeros;
}

std::vector<int> readInput() {
    std::vector<int> xs {};

    std::string line;
    int distance;

    while (std::getline(std::cin, line)) {
        distance = std::stoi(line.substr(1));
        xs.push_back(line[0] == 'L' ? -distance : distance);
    }
    return xs;
}

int main() {
    std::vector<int> xs = readInput();
    std::cout << "Star 1: " << star1(xs) << "\n";
    std::cout << "Star 2: " << star2(xs) << "\n";

    return 0;
}
