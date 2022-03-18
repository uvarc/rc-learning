#include <iostream>
#include <cmath>

using namespace std;

double euclidean(double x1, double y1, double x2, double y2) {
    return sqrt(pow(x1-x2, 2) + pow(y1-y2, 2));
}

void closer_to_third(double x1, double y1, double x2, double y2, double x3, double y3) {
    double dist1=euclidean(x1,y1,x3,y3);
    double dist2=euclidean(x2,y2,x3,y3);
    if (dist1 < dist2) {
        cout << "1 is closer to 3\n";
    } else if (dist1 > dist2) {
        cout << "2 is closer to 3\n";
    } else {
        cout << "1 and 2 are the same distance from 3\n";
    }
}

int main() {

    double x1,y1,x2,y2;

    x1=-1, y1=2, x2=3, y2=5;
    cout << euclidean(x1,y1,x2,y2) << "\n";

    x1=11,y1=4, x2=7, y2=9;
    cout << euclidean(x1,y1,x2,y2) << "\n";

    int x3=10, y3=5;
    closer_to_third(x1,y1,x2,y2,x3,y3);

    x1=-1, y1=2, x2=1, y2=-2;
    closer_to_third(x1,y1,x2,y2,x3,y3);
}
