#include <iostream>
using namespace std;

template <class T> 
T sum(T a, T b) {
  T result=a+b;
  return result;
}

int main() {
  cout << sum<int>(9,14) << '\n';
  cout << sum<double>(2.0,3.5) << '\n';
  return 0;
}
