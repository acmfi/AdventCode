#include <iostream>
#include <fstream>
#include <vector>
#include <string>
#include <cmath>
#include <algorithm>

int readInput(std::ifstream *myFile, std::vector<std::string> *passes);

int main(int argc, char **argv) {

  std::ifstream myFile;
  std::vector<std::string> passes;
  std::vector<int> idList;
  int maxID = 0;

  if (argc != 2) {
    std::cout << "Use: day05 input.txt" << "\n";
    return 1;
  }

  myFile.open(argv[1],std::ios::in);
  if(readInput(&myFile, &passes)){
    std::cout << "Error reading file" << "\n";
    return 1;
  }
  myFile.close();


  for(size_t i = 0; i < passes.size(); i++) {
    int ID;
    int minR = 0, maxR = 127;
    int minC = 0, maxC = 8;
    size_t x;
    std::string seat = passes[i];

    for(x = 0; x < seat.length()-3; x++) {
      switch(seat[x]) {
      case 'F': maxR -= (maxR-minR+1)/2;break;
      case 'B': minR += (maxR-minR+1)/2; break;
      default:  std::cout << "Wrong code on the seat '" << seat << "'\n";
      }
    }

    for (; x < seat.length(); x++) {
      switch(seat[x]) {
      case 'L': maxC -= (maxC-minC)/2; break;
      case 'R': minC += (maxC-minC)/2; break;
      default:  std::cout << "Wrong code on the seat '" << seat << "'\n";
      }
    }
    idList.push_back(ID = (minR*8+minC));
    if (maxID < ID) {
      maxID = ID;
    }
  }
  std::cout << "1st STAR SOLUTION -> " << maxID << "\n";
  std::sort(idList.begin(), idList.end());

  for(size_t i = 0; i < idList.size(); i++) {
    if (idList[i]+1 != idList[i+1]) {
      std::cout << "2nd STAR SOLUTION -> " << idList[i]+1 << "\n";
      break;
    }
  }

  return 0;
}


int readInput(std::ifstream *myFile, std::vector<std::string> *passes) {
  std::string line;
  if(myFile->is_open()) {
    while(*myFile >> line) {
      passes->push_back(line);
    }
    return 0;
  }else {
    return 1;
  }
}
