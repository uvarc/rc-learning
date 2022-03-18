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

class birdData {
	
//Input values.
    std::string commonName;
    std::vector<float> observations;

    public:
    birdData(std::string, std::vector<float>);
    std::string species_name();
    std::vector<float> stats(); 
    std::vector<float> minmax(std::vector<int> years); 
};

