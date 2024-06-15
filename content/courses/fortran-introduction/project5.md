---
title: "Project 5"
date : "2021-04-5T00:00:00-05:00"
toc: true
type: book
weight: 74
---

Download the file [vabirds.csv](/data/vabirds.csv).
1. Create the derived type bird_data in a module bird_dat as illustrated in the example.  This is still a type; we do not need a class for this exercise. 
2. Add the following procedures to the constructor already written.
   * Write a `stats` procedure that takes only an instance of the type and returns the mean and standard deviation of the observations for that instance.
   * Write a minmax procedure that takes an instance of the type and the array of years and returns the maximum observed, the minimum observed, and the years for maximum and minimum.  You may use the maxval, minval, maxloc, and minloc intrinsics.
   * Write a main program that uses your module and also uses the sorters module that you can download ([sorters.f90](/courses/fortran-introduction/solns/sorters.f90)). This implements bubblesort.  Bubblesort is simple and slow but is more than sufficient for this exercise.  Note that the subprogram is destructive, i.e. it overwrites the array to be sorted, so make a copy if you don’t want that.

Remember to write an explicit interface for each subprogram in this “main” file.  Do not use CONTAINS. Read the file name from the command line.   First of all you will need to count the number of lines in the file.  Write a function count_lines that does this and returns the number.  It is up to you whether you pass it the number of header/footer lines.
Count_lines can check for the existence of the file and return 0 if it is not found.
Still in the read_data routine, using the number of items in the file, corrected for the header and the two footers, allocate an array of bird_data types.  
Loop through this array calling your constructor for each species.  
The read_data routine should return the array of years and the array of bird_data types.  

Request a species name from the user.  Find the species in your array of types 
and print its mean, standard deviation, and results from minmax. Print some appropriate message if the species is not found.  Compute an array of the means for all species.  
Use the pbsort routine from `sorters` to sort this array.  This procedure also returns the permutation vector, which is an array of the indices of the original positions.  
For example, if after the sort the permutation vector is (17,3,55,11,23, and so forth) that means that the element that was previously 17 is now the first in the new array, and so on.  
Note that these sorters return in ascending order (smallest to largest).  From the sorted mean array and the permutation index, print the names of the 10 most common (by mean) species over the years of observations.  
Hint: you can use a trick to reverse a dimension of an array in Fortran: 
```fortran
R=A(ndim:1:-1)
```
Test the user input portion for 
<br>
TurkeyVulture
<br>
TuftedTitmouse 
<br>
ElegantTrogon

For this project you can require an exact match of the species name.  (Note that no spaces are allowed and words are separated by capitalization; we would have to do more sophisticated string handling if we were to allow spaces and variations in capitalization.)

In addition to the sorters.f90 module mentioned above, the sample solution uses the [file_utils](/courses/fortran-introduction/solns/file_utils.f90) module that collects some useful file-related subprograms, including the count_lines function.

{{< spoiler text="Sample solution" >}}
{{< code-download file="/courses/fortran-introduction/solns/bird_dat.f90" lang="fortran" >}}
{{< code-download file="/courses/fortran-introduction/solns/bird_obs.f90" lang="fortran" >}}
{{< /spoiler >}}
