// skipws flag example
#include <iostream>     // std::cout, std::skipws, std::noskipws
#include <sstream>      // std::istringstream
#include <string>

int main () {
  //std::string a, b, c;
  char a, b, c;

  std::istringstream iss ("  123 11 12");
  iss >> a >> b >> c;
  std::cout << a <<" " << b << " "<< c << '\n';

  iss >> std::skipws >> a >> b >> c;
  std::cout << a <<" " << b << " "<< c << '\n';

  iss.seekg(0);
  iss >> std::noskipws >> a >> b >> c;
  std::cout << a <<" " << b << " "<< c << '\n';

  iss.seekg(0);
  iss >> std::ws >> a >> b >> c;
  std::cout << a <<" " << b << " "<< c << '\n';
  return 0;
}
