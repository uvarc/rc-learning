---
title: Project Set 1
toc: true
type: docs
draft: false
weight: 49

menu: 
    python-introduction:
   
---

Basic operations, lists, conditionals, loops.

## Project 1

Write a program that:

1. Creates a list of temperatures [0, 10, 20, 30, 40, 50].
2. Prints the number of items in the list.
3. Prints the index of temperature 30.
4. Adds another temperature 60 at the end.
5. Loops through the list of temperatures and converts them from Celsius to
Fahrenheit, printing each value in degrees C and F.

{{< spoiler text="Example solution" >}}
{{< code-download file="/courses/python-introduction/solns/proj_set_1/temperature_list.py" lang="python" >}}
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
{{< code-download file="/courses/python-introduction/solns/proj_set_1/temperature_loop.py" lang="python" >}}
{{< /spoiler >}}

## Project 3

Write a program that:

1. Generates a list of temperatures from -40 to 100 inclusive by increments of 5 degrees Celsius. 
2. Creates another list of the corresponding Fahrenheit temperatures. 
3. Creates a list of temperatures in degrees Fahrenheit which are greater than zero but for which the corresponding temperature in Celsius is less than zero. Print the elements of this last list.

{{< spoiler text="Example solution" >}}
{{< code-download file="/courses/python-introduction/solns/proj_set_1/temperature_cond_loop.py" lang="python" >}}
{{< /spoiler >}}

## Project 4

The Collatz conjecture is a fun little exercise in number theory.

1. Given a positive integer, if it is odd multiply it by 3 and add 1. If it is even divide by 2.
2. Repeat this procedure until the result is 1.

The Collatz conjecture is that the sequence will always reach 1. No exceptions have been found...yet.  The number of steps required to reach 1 is called the stopping time.

**A.** Write a program that will find and print the stopping time for the first N positive integers. Count the starting number itself as one of the steps. Print a table of N and stopping time.
Test your program for N=30 and N=50.

**B.** Modify your program to print the starting number, its stopping time, and the maximum value of the sequence of numbers. **Hint:** If you use a list you will be able to use the len() and max() intrinsic (built-in) functions. Confirm that you get the same stopping numbers as before. Note: the example solution uses some capabilities of [printing](/courses/python-introduction/formatted_io) in Python 3 that we have not yet encountered, in order to make the output neater and easier to read.  Do not worry about aligning your results at this point.

{{< spoiler text="Example solution" >}}
{{< code-download file="/courses/python-introduction/solns/proj_set_1/collatz.py" lang="python" >}}
{{< /spoiler >}}

## Project 5

The algorithm for converting an integer N in base 10 to another base is as follows:

1. Find the remainder of N divided by the target base.
2. Divide N by the target base using integer division. If the result is greater than zero, replace the previous value of N by the result of the integer division. Store the remainder previously obtained as the new leftmost digit for the number in the target base. Repeat until the result of the integer division is 0.

**A.** Write a program to convert the first 51 integers, starting at 0 and ending at 50, to octal (base 8). Print a table of the decimal number and its octal equivalent.
**Hint:** construct a list of digits as you work through the integer divisions. The elements of the list should be strings so you’ll need to convert from integer to string. To change from a list of individual strings to a single string for printing, use the join function as follows:
 "". join(digits)
That is two (regular, not “smart”) double quotes with nothing between them, followed by a period, followed by join and in parentheses, the name of the list you have created.

**B.** Modify your program to handle bases up to 16 (hexadecimal). Use the letters of the alphabet to represent digits 10, 11, 12, ... as A, B, C, ... Hint: the char(<number>) built-in converts from an integer to its representation in the ASCII collating sequence. Note that A is number 65, i.e. chr(65)="A". The rest of the alphabet follows in numerical sequence to 96, then the lower-case letters begin at 97. Please use upper case letters.
The only widely used base greater than 10 is hexadecimal (base 16). Print a table of 0 to 32 as hexadecimal numbers.  Play with formatting to produce a nice table (use spaces, dashes, and the like).  The solution demonstrates controlling the appearance of printed output with format strings.  Take a look back when you have studied formatted output.

{{< spoiler text="Example solution" >}}
{{< code-download file="/courses/python-introduction/solns/proj_set_1/base_convert.py" lang="python" >}}
{{< /spoiler >}}
