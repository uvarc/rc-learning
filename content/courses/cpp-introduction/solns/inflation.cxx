/*****************************************************************************
Author: K. Holcomb
Date:   February 2017

File Name:   inflation.cxx

Description: This program computes inflation factors between two years.  
             
Input:       Inflation data from a file, and one or two years.
             
Output:      Inflation factor between a specified year and 2020
******************************************************************************/
#include <iostream>
#include <fstream>
#include <iomanip>
#include <string>
#include <sstream>

using namespace std;

int main(int argc, char **argv)
{
    //Declare Variables
    string inFile, inputYear1="", inputAmount="";
    ifstream fin;
    int year1=0;
    float amount=0;
    string line;

    //Get input
    if ( argc==3 ) {
       inFile=argv[1];
       inputYear1=string(argv[2]);
       stringstream ss1(inputYear1);
       ss1>>year1;
    }
    else if ( argc==4) {
       inFile=argv[1];
       inputYear1=string(argv[2]);
       stringstream ss1(inputYear1);
       ss1>>year1;
       inputAmount=string(argv[3]);
       stringstream ss2(inputAmount);
       ss2>>amount;
    }
    else {
       cout<<"Usage: datafile year <amount>\n";
       return 1;
    }

    //Read data.  Should do more checking here.
    fin.open(inFile.c_str());
    int lineCount=0;
    
    if ( fin.is_open() ) {
        while (getline(fin,line)) {
            lineCount++;
        }
    }

    fin.close();
    fin.open(inFile.c_str());

    //Data arrays (one header line)
    int nyears=lineCount-1;
    int   * year=new int[nyears];
    float * cpi =new float[nyears];

    //Skip header
    getline(fin,line);

    int lineNo=0;
    while ( getline(fin,line) ) {
        stringstream lineStream(line);
        string * lineVals=new string[2];
        int index=0;
        while ( getline(lineStream, lineVals[index],',') ) {
            ++index;
        }
        year[lineNo]=atoi(lineVals[0].c_str());
        cpi[lineNo]=atof(lineVals[1].c_str());
        lineNo++;
    }

    if (year1<year[0] || year1 > year[nyears-1]) {
        cout<<"Requested year outside data range."<<endl;
        return 2;
    }

    int baseYear=year[0];

    int index=year1-baseYear;
    float factor=cpi[nyears-1]/cpi[index];

    if (amount==0) {
    cout<<"The inflation factor since "<<year1<<" is "
        <<setiosflags(ios::fixed)<<setprecision(2)
        <<factor<<".\n";
    }
    else {
    cout<<amount<<" in "<<year1<<" is "
        <<setiosflags(ios::fixed)<<setprecision(2)
        <<amount*factor<<" in current dollars.\n";
    }

    //Make file
    float * inflation = new float[nyears-1];

    for (int n=0; n<nyears-1; ++n) {
        inflation[n]=(cpi[n+1]-cpi[n])/12.;
    }

    ofstream fout;
    fout.open("inflation.csv");
    for (int i=0;i<nyears-1;++i) {
        fout<<year[i]<<","<<inflation[i]<<"\n";
    }

    return 0;
}
