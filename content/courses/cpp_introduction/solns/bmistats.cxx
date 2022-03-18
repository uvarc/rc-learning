#include<iostream>
using namespace std;
float convert_weight(float lbs) {
    float lbs2kgs=0.453592;
    return (lbs*lbs2kgs);
}

float convert_height(float feet, float inches) {
    float inch2cm=2.54;
    float ms=(feet*12.+inches)*inch2cm;
    return (ms);
}

float calculate_bmi(float ht, float wt) {
    return (wt/(ht*ht));
}

int bmi_table(float bmi) {
    float bounds[6]={16.0,18.5,25.,30.,35.,40.};

    int nCategories=7;

    if ( bmi < bounds[0] ) return 1;
    else if (bounds[0]<=bmi && bmi<bounds[1]) return 2;
    else if (bounds[1]<=bmi && bmi<bounds[2]) return 3;
    else if (bounds[2]<=bmi && bmi<bounds[3]) return 4;
    else if (bounds[3]<=bmi && bmi<bounds[4]) return 5;
    else if (bounds[4]<=bmi && bmi<bounds[5]) return 6;
    else return 7;
    //if ( bmi>=bounds[nCategories-1] ) return nCategories;
}
