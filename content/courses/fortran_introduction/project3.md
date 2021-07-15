---
title: "Project 3"
toc: true
type: book
weight: 54

menu:
    fortran_introduction:
        parent: Project 3
        weight: 54
---

Download the [cpi.csv](/data/cpi.csv) file. Write a program to read this file 
and compute the inflation rate.
Do not assume you know in advance how many lines the file contains.  Note that the first line of the file is a header.
The derivative of the CPI will give us a crude estimate of the inflation rate. The CPI data provided is annual so let us use the formula
$$ I=CPI(yr+1)-CPI(yr)/12.$$
Notice that you always have one less year of inflation than of CPIs.  Compute the inflation array and write it to a file `inflation.csv`.  Plot the CPI and the inflation using anything you know (Excel, Python, Matlab, etc.).

{{< spoiler text="Sample solution" >}}
{{< code file="/courses/fortran_introduction/solns/inflation.f90" lang="fortran" >}}
{{< /spoiler >}}
