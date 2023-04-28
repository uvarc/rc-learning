#include <iostream>

using namespace std;

int main() {
    int a[10];
    int size = sizeof(a)/sizeof(a[0]);
    cout << size << "\n";
    a[3]=11;
    for (int i=0; i<size; ++i) {
        cout << a[i] << " ";
    }
    cout << "\n\n";

    int m=4, n=5;
    float **A;
    A=new float*[n];
    for (int i=0; i<m; ++i) {
        A[i]=new float[n];
    }
    for (int i=0; i<m; ++i) {
        for (int j=0; j<n; ++j) {
            A[i][j] = i+j;
        }
    }
    for (int i=0; i<m; ++i) {
        for (int j=0; j<n; ++j) {
            cout << A[i][j] << " ";
        }
        cout << "\n";
    }
}
