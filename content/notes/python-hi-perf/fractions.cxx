#include <iostream>

using namespace std;

class Fraction {
    public:
       int num, denom;
       Fraction(int n, int d);
       Fraction addFracs(Fraction f1, Fraction f2);
};

Fraction::Fraction(int n, int d) {
    if ( d != 0 ) {
       num=n;
       denom=d;
    } 
    else {
       num=0; denom=1;
    }
}

Fraction addFracs(Fraction f1, Fraction f2) {
    int dm=f1.denom*f2.denom;
    Fraction f3(f1.num*f2.denom+f1.denom*f2.num,dm);
    return f3;
}
