#include <iostream>
#include <fstream>
#include <string>
#include <vector>

int run(std::vector<int> code) {
  int index = 0;
  int ptr, op;
  while((ptr = index * 4) < code.size()) {
    op = code[ptr];
    if(op == 99) break;
    if(op == 1)
      code[code[ptr+3]] = code[code[ptr+1]] + code[code[ptr+2]];
    else if(op == 2)
      code[code[ptr+3]] = code[code[ptr+1]] * code[code[ptr+2]];
    else throw "Kaboom";
    index++;
  }

  return code[0];
}

int main() {
  std::ifstream inputfile("input.txt");
  std::string line;
  std::vector<int> originalcode;
  int num, result;
  while (inputfile >> num) {
    originalcode.push_back(num);
    if(inputfile.peek() == ',')
      inputfile.ignore();
  }

  std::vector<int> code = originalcode;

  code[1] = 12;
  code[2] = 2;
  
  try {
    result = run(code);
  } catch (std::exception e) {
    std::cout << "Unexpected opcode in part 1" << std::endl;
    return 1;
  }
  std::cout << "Result: " << result << std::endl;

  try {
    for(int noun = 0; noun < 100; noun++) {
      for(int verb = 0; verb < 100; verb++) {
        code = originalcode;
        code[1] = noun;
        code[2] = verb;
        if(run(code) == 19690720) {
          std::cout << "Result 2: " << 100 * noun + verb << std::endl;
        }
      }
    }  
  } catch (std::exception e) {
      std::cout << "Unexpected opcode in part 2" << std::endl;
        return 1;
  }

  return 1;
}