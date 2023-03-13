#include <iostream>
#include <string>
#include <sstream>

int main(int argc, char **argv) {

  std::string filename;
  float value;

  if (argc>2) {
     filename=argv[1];
     std::stringstream inputValue;
     inputValue<<argv[2];
     inputValue>>value;
     std::cout<<"Input is "<<filename<<" "<<value<<"\n";
   } else {
     std::cout<<"Usage: filename number\n";
     exit(1);
   }
   return 0;
}
