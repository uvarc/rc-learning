---
title: "Project 5"
toc: true
type: book
weight: 84
---

Download the file [vabirds.csv](/data/vabirds.csv).
1. Create a struct birdData in files birdData.h and birdData.cxx. 
2. Write the following functions, implemented in birdDat.cxx. 
  * Write a `stats` procedure that takes only an instance of the struct and returns the mean and standard deviation of the observations for that instance. Hint: one way to return multiple items in C++ is to use a vector.
  * Write a minmax procedure that takes an instance of the type and the array of years and returns the maximum observed, the minimum observed, and the years for maximum and minimum.  You may use the maxval, minval, maxloc, and minloc intrinsics.
   * Write a main program that uses your struct.  Look up the `sort` 

Write a read_data routine that uses the number of items in the file, corrected for the header and the two footers, to allocate a vector of bird_data types.  
Loop through this vector to load the data for each species.  
The read_data routine should return the vector of years and the vector of bird_data types.  

Request a species name from the user.  Find the species in your vector of types 
and print its mean, standard deviation, and results from minmax. Print some appropriate message if the species is not found.  Compute a vector of the means for all species.  
Use the built-in `sort` to sort the means vector.  However, we also want the permutation vector, which is a vector of the indices of the original positions.  
For example, if after the sort the permutation vector is (17,3,55,11,23, and so forth) that means that the element that was previously 17 is now the first in the new array, and so on.  
Using online resources, figure out how to do this.  Hint: you will need to use a "custom comparitor."

Test the user input portion for 
<br>
TurkeyVulture
<br>
TuftedTitmouse 
<br>
ElegantTrogon

For this project you can require an exact match of the species name.  (Note that no spaces are allowed and words are separated by capitalization; we would have to do more sophisticated string handling if we were to allow spaces and variations in capitalization.)

{{< spoiler text="Sample solution" >}}
{{< code-download file="/courses/cpp-introduction/solns/birdstruct/birdData.h" lang="c++" >}}
{{< code-download file="/courses/cpp-introduction/solns/birdstruct/birdData.cxx" lang="c++" >}}
{{< code-download file="/courses/cpp-introduction/solns/birdstruct/birdstats.cxx" lang="c++" >}}
{{< /spoiler >}}
