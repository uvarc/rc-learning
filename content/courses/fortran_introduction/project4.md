---
title: "Project 4"
toc: true
type: book
weight: 56

menu:
    fortran_introduction:
        parent: Project 4
        weight: 56
---

Using the [cpi.csv](/data/cpi.csv) data from [Project 3](/courses/fortran_introduction/project3), write a program that will read from the command line the name of a file. Read this file into your program.  Request from the user a year (use non-advancing IO). Once again, do not assume you know in advance how many lines the file contains. Check that you have enough command line input. Stop with a message if you don’t have enough command line values. Use the inquire statement to check that the file exists before you attempt to open it. You can add a message to stop, e.g. stop “Invalid file”.  Test that the file can be opened as part of your open statement.

The ratio of any two years is an estimate of the change in the cost of living.  Compute the change in the cost of living from the year you specify to 2020. Print it out neatly with some informative text. Do not print more than 2 decimal pla

In 1954 a color television cost $1295. From your result how much would that be in 2020 dollars?

{{< spoiler text="Sample solution" >}}
{{< code file="/courses/fortran_introduction/solns/cpi.f90" lang="fortran" >}}
{{< /spoiler >}}
