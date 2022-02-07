---
title: Project Set 1
toc: true
type: book
draft: false
weight: 65
---

## Project 1

Write a program that:

1. Creates a list of temperatures [0, 10, 20, 30, 40, 50].
2. Prints the number of items in the list.
3. Prints the index of temperature 30.
4. Adds another temperature 60 at the end.
5. Loops through the list of temperatures and converts them from Celsius to
Fahrenheit, printing each value in degrees C and F (use print C,F).

{{< spoiler text="Example solution" >}}
{{< code-download file="/courses/python_introduction/solns/proj1.py" lang="python" >}}
{{< /spoiler >}}

## Project 2

Write a program that:

1. Creates the temperature list 0 through 60 by 10 using a loop rather than by
typing in all the values.
2. As each degree C value is added, converts it to F and adds the F value to a list
for the Fahrenheit equivalents.
3. Makes another loop which prints out C and F similarly to Project 1, i.e. both
on the same line, but does it by indexing into the two lists.

{{< spoiler text="Example solution" >}}
{{< code-download file="/courses/python_introduction/solns/proj2.py" lang="python" >}}
{{< /spoiler >}}

## Project 3

Write a program that:

1. Generates a list of temperatures from -40 to 100 inclusive by increments of 5 degrees Celsius. 
2. Creates another list of the corresponding Fahrenheit temperatures. 
3. Creates a list of temperatures in degrees Fahrenheit which are greater than zero but for which the corresponding temperature in Celsius is less than zero. Print the elements of this last list.

{{< spoiler text="Example solution" >}}
{{< code-download file="/courses/python_introduction/solns/proj3.py" lang="python" >}}
{{< /spoiler >}}

## Project 4

Download the file [us-state-capitals.csv](/data/us-state-capitals.csv).  Write a program that will read this file and create a dictionary with the state name as the key and the capital name as the value.  Using your dictionary, print the capitals of Arkansas, Virginia, and Wyoming.

Again using your dictionary, generate a list of all state capitals that begin with the letter 'A'.  Use the list to create a string consisting of these city names separated by a semicolon ;   Open a new file capitals-with-a.txt and write this string to it.

{{< spoiler text="Example solution" >}}
{{< code-download file="/courses/python_introduction/solns/state_capitals.py" lang="python" >}}
{{< /spoiler >}}

## Project 5

Write a program that obtains the sum of the numbers from 1 to some specified positive (>0) integer N. Request the value of N as console input from the user. Your program should catch user inputs that cannot be converted to integers greater than 0.  Do not use the Gauss formula, do this via “brute force.”
Print the number, its sum as obtained from your work, and the correct answer from the Gauss formula sum(N)=N(N+1)/2.  Test your program with N=1, N=25, N=1000.

{{< spoiler text="Example solution" >}}
{{< code-download file="/courses/python_introduction/solns/proj5.py" lang="python" >}}
{{< /spoiler >}}

