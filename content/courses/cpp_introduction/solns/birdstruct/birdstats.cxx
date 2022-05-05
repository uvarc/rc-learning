/*
 * 
 * **********************************************************
 * 
 * Author: K. Holcomb
 * 
 * Changelog: Modified from old student homework 20170410
 * 
 * 
 * **********************************************************
 * 
 */

#include <iostream>
#include <cmath>
#include <fstream>
#include <string>
#include <sstream>
#include <vector>
#include <algorithm>
#include "birdData.h"

using namespace std;

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


    string userInput;
    cout<<"Please enter the name of a bird species: ";
    cin>>userInput;
    int speciesIndex=-1;
    for (int i=0;i<numSpecies;i++) {
	if (birds[i].commonName==userInput) {
            speciesIndex=i;
            break;
	}
    }
    if (speciesIndex==-1) {
	cout<<endl<<"No such species. Please start over."<<endl<<endl;
    }
    else {		
        float minVal, maxVal;
        int   minYear, maxYear;
	cout<<endl<<"The "<<userInput<<" species was observed an average of ";
        vector<float> birdStats=stats(birds[speciesIndex]);
        float mean=birdStats[0]; float stdev=birdStats[1];
        cout<<mean<<" times, ";
	cout<<"with a"<<endl<<"standard deviation of "<<stdev;
        vector<float> yearObs=minmax(birds[speciesIndex],years);
        minVal=yearObs[0]; maxVal=yearObs[1];
        minYear=(int) yearObs[2]; maxYear=(int) yearObs[3];
	cout<<" a maximum of "<<maxVal<<" in year "<<maxYear;;
	cout<<", and a minimum of "<<minVal<<" in year "<<minYear<<".\n";
    }

    vector<float> means(numSpecies);
    for (int i=0;i<numSpecies;i++) {
	means[i]=stats(birds[i])[0];
    }
    int mostCommon=10;
    int tenCommonIndex=numSpecies-mostCommon;				

    vector<int> index(numSpecies);
    for (int i=0;i<numSpecies;++i) {
        index[i]=i;
    }

    sort(index.begin(), index.end(), [&](const int& a, const int& b) {
        return (means[a] < means[b]); });
			
    cout<<"The 10 most common species (in order of decreasing ";
    cout<<"prevalence) are:"<<endl;
    int j;
    for (int i=0;i<mostCommon;i++) {
        j=numSpecies-i-1;
        cout<<birds[index[j]].commonName<<"    \t"<<means[index[j]]<<endl;
    }
    cout<<endl<<endl;

    return 0;

}
