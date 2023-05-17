#include <iostream>
#include <fstream>
#include <string>
#include <sstream>
#include <vector>

using namespace std;

float inflationFactor(float* cpi, int baseYear, int compYear1, int compYear2=1980);

int main(int argc, char **argv)
{
    //Declare Variables
    ifstream fin;
    string line;

    vector<int> year;
    vector<float> cpi;

    fin.open("cpi.csv");

    getline(fin,line);
    while ( getline(fin,line) ) {
        stringstream lineStream(line);
        string *lineVals=new string[2];
        int index=0;
        while ( getline(lineStream, lineVals[index],',') ) {
            ++index;
        }
        year.push_back(atoi(lineVals[0].c_str()));
        cpi.push_back(atof(lineVals[1].c_str()));
    }

    return 0;
}
