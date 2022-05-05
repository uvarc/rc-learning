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
#include <iostream>
#include "birdData.h"

using namespace std;

vector<float> stats(birdData bird) {
    float sum=0,devSum=0;
    int yearsObs=bird.observations.size();
    for (int i=0;i<yearsObs;i++) {
        sum+=bird.observations[i];
    }

    float mean=sum/yearsObs;
    for (int i=0;i<yearsObs;i++) {
         devSum+=pow((mean-bird.observations[i]),2);
   }

   float stdev=sqrt(devSum/yearsObs);
   vector<float> birdStats={mean,stdev};
   return birdStats;
}

vector<float> minmax(birdData bird,vector <int> years) {
    int minIndex=0, maxIndex=0;
    float minimum=bird.observations[0];
    float maximum=bird.observations[0];
    int yearsObs=years.size();
    for (int i=1;i<yearsObs;i++) {
    //If multiple occurrences, will return last one.
        if (bird.observations[i]<=minimum) {
            minimum=bird.observations[i];
            minIndex=i;
        }
        if (bird.observations[i]>=maximum) {
            maximum=bird.observations[i];
            maxIndex=i;
        }
    }

    vector<float> minmaxes={minimum,maximum,(float) years[minIndex],(float) years[maxIndex]};
    return minmaxes;
}
