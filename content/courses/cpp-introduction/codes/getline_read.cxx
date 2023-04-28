#include <fstream>
#include <iostream>
#include <string>

int main() {

  std::string line;
  //note implicit open
  std::ifstream mystream("data.txt");
  if (mystream.is_open()) {
      while (std::getline(mystream,line)) {
      //do something with line
      std::cout<<line<<"\n";
      }
  }
  else {
     std::cout<<"Unable to open file\n";
  }

}
      

