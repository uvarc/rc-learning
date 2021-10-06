#include <iostream>
#include <fstream>
#include <sstream>

int main() {

    const int nobs=200;
    float bf[nobs],wt[nobs],ht[nobs];
    string line;

    ifstream fin("datafile.txt");
    if (fin.is_open()) {
         while (getline(fin,line)) {
             stringstream lineStream(line);
             string *linevals=new string[4];
             int index=0;
             while (getline(lineStream,linevals[index],',') ) {
                 ++index;
             }
             stringstream ssbf,sswt,ssht;
             ssbf<<linevals[0];
             ssbf>>bf[lineCount];
             sswt<<linevals[2];
             ssht>>ht[lineCount];
             lineCount++;
         }
    } else {
        cout<<"Unable to open file";
        return 1;
    }

    return 0;

}

