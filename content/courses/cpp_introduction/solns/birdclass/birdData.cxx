/*
 * 
 * **********************************************************
 * 
 * Author: K. Holcomb
 *
 * 
 * Changelog: Modified from old student homework 20170410
 * 
 * **********************************************************
 * 
 */

#include <cmath>
#include <string>
#include <vector>
#include "birdData.h"

using namespace std;

birdData::birdData(string speciesName,vector<float>obs){
    commonName=speciesName;
    int yearsObs=obs.size();
    for (int i=0;i<yearsObs;i++) {
        observations.push_back(obs[i]);
    }
    return;
}

//Accessor
string birdData::species_name(){
    return this->commonName;
}

vector<float> birdData::stats() {
    float sum=0,devSum=0;
    int yearsObs=observations.size();
    for (int i=0;i<yearsObs;i++) {
        sum+=observations[i];
    }

    float mean=sum/yearsObs;
    for (int i=0;i<yearsObs;i++) {
        devSum+=pow((mean-observations[i]),2);
    }

    float stdev=sqrt(devSum/yearsObs);
    vector<float> birdStats={mean,stdev};
    return birdStats;
}

vector<float> birdData::minmax(vector <int> years) {
    int minIndex=0, maxIndex=0;
    float minimum=observations[0];
    float maximum=observations[0];
    int yearsObs=years.size();
    for (int i=1;i<yearsObs;i++) {
    //If multiple occurrences, will return last one.
        if (observations[i]<=minimum) {
            minimum=observations[i];
            minIndex=i;
        }
        if (observations[i]>=maximum) {
            maximum=observations[i];
            maxIndex=i;
        }
    }

    vector<float> minmaxes={minimum,maximum,(float) years[minIndex],(float) years[maxIndex]};
    return minmaxes;
}
