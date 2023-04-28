#include <iostream>

using namespace std;

class Fraction {
    int num, denom;
    int gcd(int, int);
    Fraction reduce(int, int);
    friend ostream& operator<<(ostream&, const Fraction&);

    public:
       Fraction(int n, int d);
       Fraction operator+(const Fraction &);
       Fraction operator-(const Fraction &);
       Fraction operator*(const Fraction &);
       Fraction operator/(const Fraction &);
       Fraction& operator=(const Fraction &);
       void printme();
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

void Fraction::printme() {
    cout<<this->num<<"/"<<this->denom<<endl;
}

int Fraction::gcd(int n, int d) {
    int  cd, den, temp;
    cd=n;
    den=d;
    while (den !=0) {
        temp=den;
        den=cd%den;
        cd=temp;
    }
    return cd;
}

Fraction Fraction::reduce(int n, int d) {
    int g_c_d = gcd(n, d);
    n/=g_c_d;
    d/=g_c_d;
    Fraction f3=Fraction(n,d);
    return f3;
}

Fraction Fraction::operator+(const Fraction &f2) {
    if ( this->denom != 0 && f2.denom != 0 ) {
        int dm=this->denom*f2.denom;
        Fraction f3=reduce(this->num*f2.denom+this->denom*f2.num,dm);
        return f3;
    }
    else {
        Fraction f3(0,0);
        return f3;
    }
}

Fraction Fraction::operator-(const Fraction &f2) {
    if ( this->denom != 0 && f2.denom != 0 ) {
        int dm=this->denom*f2.denom;
        Fraction f3=reduce(this->num*f2.denom-this->denom*f2.num,dm);
        return f3;
    }
    else {
        Fraction f3(0,0);
        return f3;
    }
}

Fraction Fraction::operator*(const Fraction &f2) {
    if ( this->denom != 0 && f2.denom != 0 ) {
        int dm=this->denom*f2.denom;
        int nm=this->num*f2.num;
        Fraction f3=reduce(nm,dm);
        return f3;
    }
    else {
        Fraction f3(0,0);
        return f3;
    }
}

Fraction Fraction::operator/(const Fraction &f2) {
    if ( this->denom != 0 && f2.denom != 0 ) {
        int dm=this->denom*f2.num;
        int nm=this->num*f2.denom;
        Fraction f3=reduce(nm,dm);
        return f3;
    }
    else {
        Fraction f3(0,0);
        return f3;
    }
}

Fraction& Fraction::operator=(const Fraction &f2) {
    if (this==&f2) return *this;
    num=f2.num;
    denom=f2.denom;
    return *this;
}

ostream& operator<<(ostream& os, const Fraction & f2) {
    os<<f2.num<<"/"<<f2.denom;
    return os;
}

int main() {
    int n1=1, n2=1, d1=2, d2=4;
    Fraction f1(n1,d1);
    Fraction f2(n2,d2);
    Fraction f3=f1+f2;
    Fraction f4=f1-f2;
    Fraction f5=f1*f2;
    Fraction f6=f1/f2;
    Fraction f7=f5;

    f1.printme();
    f2.printme();
    f3.printme();
    f4.printme();
    f5.printme();
    f6.printme();
    f7.printme();

    cout<<f2<<"\n";

    return 0;
}
