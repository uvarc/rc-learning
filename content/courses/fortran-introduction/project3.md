---
title: "Project 3"
date : "2021-04-5T00:00:00-05:00"
toc: true
type: book
weight: 56
---

Using the [cpi.csv](/data/cpi.csv) data, write a program that will read from the command line the name of a file. Read this file into your program.  Request from the command line a year.  Optionally request on the command line an amount.
Check that you have enough command line input. Stop with a message if you don’t have enough command line values. 

Do not assume you know in advance how many lines the file contains. 
Use the inquire statement to check that the file exists before you attempt to open it. You can add a message to stop, e.g. stop “Invalid file”.  Test that the file can be opened as part of your open statement.

Once the data have been read, check that the requested year is in range.

The ratio of any two years is an estimate of the change in the cost of living.  Compute the change in the cost of living from the year you specify to the last number in the data (2020 in the sample file). Print it out neatly with some informative text. Print the result to 2 decimal places.

In 1954 a color television cost $1295. From your result how much would that be in 2020 dollars?

A rough estimate of the year-over-year inflation rate can be obtained from
```fortran
inflation(i)=(cpi(i+1)-cpi(i))/12.
```
Compute the inflation rate for the data and print with the corresponding year to a comma-separated file.  Use any plotting package with which you are familiar (Excel, Matlab, Python, R, etc.) to plot the data.

{{< spoiler text="Sample solution" >}}
{{< code-download file="/courses/fortran-introduction/solns/inflation.f90" lang="fortran" >}}
{{< /spoiler >}}
