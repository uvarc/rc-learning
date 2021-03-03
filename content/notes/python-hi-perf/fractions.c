typedef struct {
   int num, denom;
} Fraction;

Fraction createFract(int n, int d) {
    int num, denom;
    if ( d != 0 ) {
       num=n;
       denom=d;
    } 
    else {
       num=0; denom=1;
    }
    Fraction fract={num,denom};
    return fract;
}

Fraction addFracs(Fraction f1, Fraction f2) {
    int dm=f1.denom*f2.denom;
    Fraction f3=createFract(f1.num*f2.denom+f1.denom*f2.num,dm);
    return f3;
}

int main() {
#include <stdio.h>
    int n1=1, n2=2, d1=3, d2=4;
    Fraction f1, f2;
    f1=createFract(n1,d1);
    f2=createFract(n2,d2);
    printf("%d %d\n",f1.num,f1.denom);
    printf("%d %d\n",f2.num,f2.denom);

    Fraction f3=addFracs(f1,f2);
    printf("%d %d\n",f3.num,f3.denom);

    return 0;
}
