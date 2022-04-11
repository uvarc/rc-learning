#include <vector>

#include <iostream>

#include "fractions.h"

Fraction::Fraction(int numerator, int denominator) {
    if ( denominator != 0 ) {
        num=numerator;
        denom=denominator;
    }
    else {
	num=0;
	denom=0;
    }
}

std::vector<int> Fraction::addFracs(Fraction f2) {
    std::vector<int> fracSum={0,0};
    int dm=this->denom*f2.denom;
    int nm=this->num*f2.denom+this->denom*f2.num;
    if (dm != 0) {
       fracSum[0]=nm;
       fracSum[1]=dm;
    }
    return fracSum;
}

/*
int main(){
    std::vector<int> fracSum;
    Fraction f1, f2;
    f1.num=11; f1.denom=13;
    f2.num=5; f2.denom=8;
    fracSum=addFracs(f1,f2);
    std::cout<<fracSum[0]<<"/"<<fracSum[1]<<"\n";
}
*/

