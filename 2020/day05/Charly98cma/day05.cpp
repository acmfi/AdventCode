#include <iostream>
#include <fstream>
#include <vector>
#include <string>
#include <cmath>
#include <algorithm>
#include <omp.h>

int readInput(std::ifstream *myFile, std::vector<std::string> *passes);


/*
 * TODO: FIX THE RESULT OF THE 1ST STAR -> reduction doesnt work properly
 */
int main(int argc, char **argv) {

  std::ifstream myFile;
  std::vector<std::string> passes;
  std::vector<int> idList;
  std::string seat;
  size_t i, x, numSteps;
  int maxR, minR, maxC, minC;
  int maxID;

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

  numSteps = passes.size();

#pragma omp parallel for shared(std::cout, passes, idList, numSteps) private(x, seat, maxR, minR, maxC, minC) lastprivate(i) reduction(max:maxID) default(none)
  for(i = 0; i < numSteps; i++) {
    minR = 0, maxR = 127;
    minC = 0, maxC = 8;
    seat = passes[i];

    for(x = 0; x < seat.length()-3; x++) {
      switch(seat[x]) {
      case 'F': maxR -= (maxR-minR+1)/2;break;
      case 'B': minR += (maxR-minR+1)/2; break;
      }
    }

    for (; x < seat.length(); x++) {
      switch(seat[x]) {
      case 'L': maxC -= (maxC-minC)/2; break;
      case 'R': minC += (maxC-minC)/2; break;
      }
    }
    maxID = (minR*8+minC);
#pragma omp critical (printID)
    std::cout << "seat ID: " << maxID << "\n";
#pragma omp critical (list_update)
    idList.push_back(maxID);
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
