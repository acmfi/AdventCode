#include <iostream>
#include <fstream>
#include <string>
#include <vector>
#include <set>

//Compile with --std=c++11
int main() {
  std::vector<int> input;
  std::string line;
  input.reserve(1000);
  std::ifstream inputfile("input.txt");
  if (inputfile.is_open())
  {
    while ( getline (inputfile,line) )
    {
      input.push_back(std::stoi(line));
      std::cout << line << std::endl;
    }
    inputfile.close();
  } else {
    std::cout << "Input file missing!" << std::endl;
    return -1;
  }
  
  int sum = 0;
  
  for( int n : input)
    sum += n;
  
  std::cout << "Ex 1 - Frequency: " << sum << std::endl;
  
  std::set<int> foundFreqs;
  sum = 0;
  
  while(true)
    for(int n : input) {
      sum += n;
      if(!foundFreqs.insert(sum).second)
        goto amievil;
    }
amievil:
  
  std::cout << "Ex 2 - Frequency repeated:" << sum << std::endl;
  
  return 0;

}