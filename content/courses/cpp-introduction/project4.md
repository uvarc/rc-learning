---
date : "2021-06-23T00:00:00-05:00"
title: "Project 4"
toc: true
type: book
weight: 75

---

Download the file [bodyfat.csv](/data/bodyfat.csv).  This is a dataset of body fat, age, height, and weight for a set of participants in a study. BMI categories are as follows:

{{< table >}}
|Severely underweight |  BMI < 16.0 |
|Underweight          | 16 <= BMI < 18.5 |
|Normal               | 18.5 <= BMI < 25 |
|Overweight           | 25 <= BMI < 30 |
|Obese Class I        | 30 <= BMI < 35 |
|Obese Class II       | 35 <= BMI < 40 |
|Obese Class III      | BMI > 40       |
{{< /table >}}

Write a `bmistats` module containing functions for the following:
1. Convert pounds to kilograms.  Use the actual conversion factor, not the approximate one.  Look it up on Google.
2. Convert feet/inches to meters.  Look up the conversion factor, do not guess at it.   
3. Compute BMI.
4. Determine where the BMI falls in the table supplied and return that informationin an appropriate form. 

Write a file `stats` that implements the following:
1. Mean of an array 
2. Standard deviation of an array 
3. Outlier rejection using Chauvenet’s criterion.  Pseudocode given further down.

Write a main program that implements the following:
1. Uses your other files
2. Reads the input file into appropriate arrays (use one-dimensional arrays for this project).  Don't assume you know the length of the file (but you can assume the number of header lines is fixed).  
3. Pass appropriate arrays to a subroutine that computes an array of BMI data based on height and weight and returns the BMI array.
4. Rejects the outlier(s).  The function should return an array of logicals that you can apply to the original data using WHERE or similar.  Create new arrays with the outlier(s) deleted. 

Write a file that contains the corrected data for bodyfat and BMI.  Use Excel or whatever you normally use to plot BMI as a function of percentage body fat. 
Be sure to plot it as a scatter plot (points only, no connecting lines).  

Chauvenet’s criterion: It’s not the state of the art but works pretty well.
1. Compute the mean and standard deviations of the observations.
2. Compute the absolute values of the deviations, i.e. abs(A-mean(A))/std(A)
3. Use the tails `devs=devs/sqrt(2.)`
4. Compute the probabilities `prob=erfc(devs)` : erfc is an intrinsic in any fairly recent Fortran compiler.  
5. The criterion is that we retain data with `prob>=1./(2*N_obs)` (number of observations).

{{< spoiler text="Example solution" >}}
{{< code-download file="/courses/cpp-introduction/solns/stats.cxx" lang="c++" >}}
{{< code-download file="/courses/cpp-introduction/solns/bmistats.h" lang="c++" >}}
{{< code-download file="/courses/cpp-introduction/solns/bmistats.cxx" lang="c++" >}}
{{< code-download file="/courses/cpp-introduction/solns/bmi.cxx" lang="c++" >}}
{{< /spoiler >}}
