#include <iostream>
#include <fstream>
#include <sstream>
#include <vector>

using namespace std;

int main() {
    fstream myfile;
    myfile.open("mydata.txt", ios::out);

    for (int i=1; i<=12; ++i) {
        myfile << i;
        if (i%3==0) {
            myfile << "\n";
        } else {
            myfile << ", ";
        }
    }
    myfile.close();

    myfile.open("mydata.txt", ios::in);

    float n[4][3];
    if (myfile.is_open()) {
        string line;
        string lineval;
	int lineCount=0;
        while (getline(myfile,line)) {
            stringstream lineStream(line);
            vector<string> linevals;
            while (getline(lineStream,lineval,',') ) {
                linevals.push_back(lineval);
            }
            stringstream ssbf,sswt,ssht;
            ssbf<<linevals[0];
            ssbf>>n[lineCount][0];
            sswt<<linevals[1];
            sswt>>n[lineCount][1];
            ssht<<linevals[2];
            ssht>>n[lineCount][2];
	    lineCount++;
        }
    } else {
        cout<<"Unable to open file";
        return 1;
    }

    for (int i=0; i<4; ++i) {
	for (int j=0; j<3; ++j) {
            n[i][j]++;
	}
    }
    for (int i=0; i<4; ++i) {
	for (int j=0; j<3; ++j) {
            cout << n[i][j] << " ";
        }
        cout << "\n";
    }
}
