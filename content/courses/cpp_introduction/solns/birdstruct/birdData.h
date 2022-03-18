/*
 * 
 * **********************************************************
 * 
 * Author: K. Holcomb
 *
 * Class describing some data about bird observations.
 * 
 * Changelog: Modified from old student homework 20170410
 * 
 * 
 * **********************************************************
 * 
 */

#include <cmath>
#include <string>
#include <vector>

using namespace std;

typedef struct {
	
//Input values.
    string commonName;
    vector<float> observations;
} birdData;

vector<float> stats(birdData); 
vector<float> minmax(birdData, vector<int>); 
