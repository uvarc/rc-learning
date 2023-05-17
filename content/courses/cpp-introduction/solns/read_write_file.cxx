#include <iostream>
#include <fstream>

using namespace std;

int main() {
    fstream myfile;
    myfile.open("mydata.txt", ios::out);

    for (int i=1; i<=12; ++i) {
        myfile << i;
        if (i%3==0) {
            myfile << "\n";
        } else {
            myfile << " ";
        }
    }
    myfile.close();

    myfile.open("mydata.txt", ios::in);

    int x[4][3];
    if (myfile.is_open()) {
        for (int i=0; i<4; ++i) {
	    myfile>>x[i][0]>>x[i][1]>>x[i][2];
        }
    } else {
        cout<<"Unable to open file";
        return 1;
    }

    for (int i=0; i<4; ++i) {
	for (int j=0; j<3; ++j) {
            x[i][j]++;
	}
    }
    for (int i=0; i<4; ++i) {
	for (int j=0; j<3; ++j) {
            cout << x[i][j] << " ";
        }
        cout << "\n";
    }
}
