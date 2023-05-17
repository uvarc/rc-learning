/*
 * bmi.cxx
 * 
 * Author:    K. Holcomb
 * Changelog: Initial version 20160308
 */

#include <iostream>
#include <fstream>
#include <string>
#include <sstream>
#include <cmath>

#include "stats.h"
#include "bmistats.h"

using namespace std;

int main(int argc, char **argv)
{
    ifstream fin;
    string bodyfat, age, wt_lbs, ht_inch;
    string inFile;
    string line;
    float zero=0.;
    const int numCategories=7;
    
    if (argc>1) {
        inFile=argv[1];
    }
    else {
        cout <<"No file name provided\n";
        return 1;
    }
    
    fin.open(inFile.c_str());
    int lineCount=0;
    if ( fin.is_open() ) {
        while (getline(fin,line)) {
            lineCount++;
        }
    }
    fin.clear();
    fin.seekg(0);

    int nobs=lineCount-1;

    float * bf=new float[nobs];
    float * wt=new float[nobs];
    float * ht=new float[nobs];

    if ( fin.is_open() ) {
        getline(fin,line);
        int lineCount=0;
        while (getline(fin,line)) {
            stringstream lineStream(line);
            string * linevals=new string[4]; //can also use vector
            int index=0;
            while ( getline(lineStream,linevals[index],',') ) {
               ++index;
            }
            stringstream ssbf, sswt, ssht;
            ssbf<<linevals[0];
            ssbf>>bf[lineCount];
            sswt<<linevals[2];
            sswt>>wt[lineCount];
            ssht<<linevals[3];
            ssht>>ht[lineCount];
                lineCount++;
        }
    }

    float * bmi=new float[nobs];
    for (int i=0;i<nobs;++i) {
        float kgs=convert_weight(wt[i]);
        //All feet are zero for this dataset
        float ht_m=0.01*convert_height(zero,ht[i]);
        bmi[i]=calculate_bmi(ht_m,kgs);
    }

    //Apply Chauvenet criterion
    bool *mask=new bool[nobs];
    int numRejected;
    numRejected=reject_outliers(bmi,mask,nobs);
    int numValid=nobs-numRejected;

    float * bodyfat_corrected=new float[numValid];
    float * bmi_corrected=new float[numValid];

    int counter=0;
    for (int i=0;i<nobs;++i) {
        if (mask[i]) {
            bmi_corrected[counter]=bmi[i];
            bodyfat_corrected[counter]=bf[i];
        counter++;
        }
    }

    //Compute and print histogram
    int bins[numCategories]={0};
    int category;
    for (int i=0;i<numValid;++i) {
        category=bmi_table(bmi_corrected[i]);
        bins[category-1]+=1;
    }

    for (int i=0;i<numCategories;++i) {
        cout<<"Category "<<i+1<<":";
        for (int j=0;j<bins[i];++j) {
           cout<<'*';
        }
        cout<<"\n";
    }

    ofstream fout;
    fout.open("corrected_data.csv");

    fout<<"Bodyfat Percentage,BMI\n";
    for (int i=0; i<numValid; ++i) {
        fout<<bodyfat_corrected[i]<<","<<bmi_corrected[i]<<"\n";
    }

    return 0;
}
