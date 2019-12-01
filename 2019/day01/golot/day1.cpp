#include <iostream>
#include <fstream>
#include <string>

int fuelrec(int mass) {
  int fuel = mass / 3 - 2;
  return fuel > 0 ? fuel + fuelrec(fuel) : 0;
}

int main() {
  int sum = 0;
  int sum2 = 0;
  std::ifstream inputfile("input.txt");
  std::string line;
  while (std::getline(inputfile, line)) {
    int mass = std::stoi(line);
    sum += mass / 3 - 2;
    sum2 += fuelrec(mass);
  }
  
  std::cout << "Fuel 1: " << sum << std::endl;
  std::cout << "Fuel 2: " << sum2 << std::endl;
  
  return 0;
}