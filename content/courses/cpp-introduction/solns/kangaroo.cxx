#include <iostream>
#include <sstream>

using namespace std;

int main() {
    int n;

    while (true) {
        cout << "Please enter an integer, 0 to quit: ";
        cin >> n;
        if (n==1) {
            cout << "zebra\n";
        } else if (n==2) {
            cout << "kangaroo\n";
        } else if (n==0) {
            break;
        } else {
            cout << "not found\n";
        }
    }
}
