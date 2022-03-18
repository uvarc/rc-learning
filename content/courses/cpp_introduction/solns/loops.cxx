#include <iostream>

int main() {

    // 1)
    for (int i=0; i<=20; i+=2) {
	    std::cout << i << ",";
    }
    std::cout << "\n\n";

    // 2)
    int n=1;
    while (n<121) {
        if (n%2==0) {
            n+=3;
        } else {
            n+=5;
        }
	std::cout << n << ",";
    }
    std::cout << "\n\n";

    // 3)
    float x=0.;
    int N=50, M=25;
    float w=9., z=13.;

    for (int i=1; i<=N; ++i) {
        if (i<M) {
            x+=11.;
        }
        if (x>w && x<z) continue;
        if (x>100) break;
    }
    std::cout << x << "\n";
}
