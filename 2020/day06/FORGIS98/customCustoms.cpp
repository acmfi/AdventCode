#include <iostream>
#include <fstream>
using namespace std;

void firstStar(ifstream& myInput){

  int total = 0;
  string line = "";
  string list = "";

  while (getline(myInput, line)) {
    if(line.empty()){
      list.clear();
    }
    else{
      for(int i = 0; i < line.size(); i++){
        if(list.find(line[i]) == string::npos){
          list += line[i];
          total += 1;
        }
      }
    }
  }

  cout << total << '\n';
}

void secondStar(ifstream& myInput){

  int total = 0;
  bool group = true;

  string line = "";
  string list = "";
  string deffList = "";

  while (getline(myInput, line)) {
    if(line.empty()){
      total += list.size();
      group = true;

      deffList.clear();
      list.clear();
    }
    else{
      for(int i = 0; i < line.size(); i++){
        if(list.find(line[i]) == string::npos && group){
          deffList += line[i];
        }
        else if(list.find(line[i]) != string::npos && !group){
          deffList += line[i];
        }
      }
      group = false;

      list = deffList;
      deffList = "";
    }
  }

  total += list.size(); // La última línea no la pilla!
  cout << total << '\n';
}

int main (int argc, char** argv) {

  ifstream myInput (argv[1], ifstream::in);

  // firstStar(myInput);
  secondStar(myInput);

  myInput.close();
  return 0;
}
