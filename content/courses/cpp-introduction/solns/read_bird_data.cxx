#include <vector>
#include <iostream>
#include <fstream>
#include <string>
#include <sstream>

using namespace std;

typedef struct {

//Input values.
    string commonName;
    vector<float> observations;
} birdData;

int main(int argc, char *argv[])
{
    ifstream inFile;
    string species;
    vector<birdData> birds;
    vector<int> years;

    float obs;
    inFile.open(argv[1]);
    if (inFile.is_open()) {
        string line,header,yearString;
        int year;
        getline(inFile,line);
        stringstream yearStream(line);
        while (getline(yearStream,yearString,',')) {
                stringstream ssyear;
                ssyear<<yearString;
                if (ssyear>>year) {
                    years.push_back(year);
                }
        }
        //Dummy for storing species strings before writing to vector.
        string speciesDatum;
        while ( getline(inFile,line) ){
            stringstream lineStream(line);
            vector<string> obsVals;
            while (getline(lineStream,speciesDatum,',')) {
                obsVals.push_back(speciesDatum);
            }
            //Create a vector of bird instances.
           birdData bird;
           bird.commonName=obsVals.at(0);
           for (int index=1;index<obsVals.size();++index){
               stringstream obsString;
               obsString<<obsVals[index];
               obsString>>obs;
               bird.observations.push_back(obs);
           }

           birds.push_back(bird);
        }
    }
    else {
        cout<<"Unable to open data file\n";
        return 1;
    }

    int numLines=birds.size();
    //Clip off last two lines
    birds.erase(birds.end()-1,birds.end());
    int numSpecies=numLines-2;
    int numObs=birds[0].observations.size()-1;

    for (int i=0;i<numSpecies;i++) {
        if (birds[i].commonName=="AmericanCrow") {
            cout<<birds[i].commonName<<"\n";
            for (int j=0;j<numObs;++j) {
	        cout<<birds[i].observations[j]<<" ";
            }
	    cout<<endl;
	}
    }

    return 0;
}

