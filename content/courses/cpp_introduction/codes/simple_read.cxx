#include <fstream>
#include <iostream>

int main() {

  float var1, var2;
  std::ifstream myfile("data.txt");
  if (myfile.is_open()) {
      while ( ! myfile.eof() ) {
          myfile >> var1 >> var2;
          std::cout<<var1<<" "<<var2<<"\n";
     }
  }
}
      

