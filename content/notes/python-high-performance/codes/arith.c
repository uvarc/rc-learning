#include "arith.h"

double sum(double x, double y) {
    return x+y;
}

double difference(double x, double y) {
    return x-y;
}

double product(double x, double y) {
    return x*y;
}

double division(double x, double y) {
    if (y != 0.) return x/y;
    else return 0.;
}
