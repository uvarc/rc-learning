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

int main() {
    int n1=1, n2=2, d1=3, d2=4;
    Fraction f1(n1,d1);
    Fraction f2(n2,d2);
    Fraction f3=addFracs(f1,f2);

    cout<<f1.num<<"/"<<f1.denom<<endl;
    cout<<f2.num<<"/"<<f2.denom<<endl;
    cout<<f3.num<<"/"<<f3.denom<<endl;

    return 0;
}
