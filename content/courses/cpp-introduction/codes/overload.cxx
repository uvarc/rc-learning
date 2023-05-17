#include <iostream>
using namespace std;

int sum(int i, int j) {
  return i+j;
}

double sum(double x, double y) {
  return x+y;
}

int main() {
  cout << sum(9,14) << '\n';
  cout << sum(2.0,3.5) << '\n';
  return 0;
}
