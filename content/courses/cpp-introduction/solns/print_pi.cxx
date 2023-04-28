#include <iostream>
#include <iomanip>
#include <cmath>

using namespace std;

int main() {

    const double pi=4*atan(1);

    cout << scientific << pi << "\n";
    cout << scientific << setprecision(8) << pi << "\n";

    return 0;
}
