---
date : "2021-06-23T00:00:00-05:00"
title: "Project 3"
toc: true
type: book
weight: 65
---

Using the [cpi.csv](/data/cpi.csv) data, write a program that will read from the command line the name of a file. Read this file into your program.  Request a year on the command line.  Optionally request from the user on the command line an amount. Check that you have enough command line input. Stop with a message if you don’t have enough command line values.

Once the data have been read, check that the requested year is in range.

The ratio of any two years is an estimate of the change in the cost of living.  Compute the change in the cost of living from the year you specify to 2020. Print it out neatly with some informative text.  Print the result to 2 decimal places.

In 1954 a color television cost $1295. From your result how much would that be in 2020 dollars?  

A rough estimate of the year-over-year inflation rate can be obtained from
```c++
inflation[i]=(cpi[i+1]-cpi[i])/12.
```
Compute the inflation rate for the data and print with the corresponding year to
 a comma-separated file.  Use any plotting package with which you are familiar (Excel, Matlab, Python, R, etc.) to plot the data.

{{< spoiler text="Sample solution" >}}
{{< code-download file="/courses/cpp-introduction/solns/inflation.cxx" lang="c++" >}}
{{< /spoiler >}}
