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
  int sum3 = 0;
  std::ifstream inputfile("input.txt");
  std::string line;
  while (std::getline(inputfile, line)) {
    int fuel = std::stoi(line) / 3 - 2;
    sum += fuel;
    sum2 += fuel + fuelrec(fuel);
    sum3 += fuel;
    while(fuel > 0) {
      fuel = fuel / 3 - 2;
      if(fuel > 0)
        sum3 += fuel;
    }
  }
  
  std::cout << "Fuel 1: " << sum << std::endl;
  std::cout << "Fuel 2 (rec): " << sum2 << std::endl;
  std::cout << "Fuel 2 (non-rec): " << sum3 << std::endl;
  
  return 0;
}