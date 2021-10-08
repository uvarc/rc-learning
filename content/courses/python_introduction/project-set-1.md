---
title: Project Set 1
toc: true
type: book
draft: false
weight: 85
---

# Beginner Level

## Project 1

Write a program that:

1. Creates a list of temperatures [0, 10, 20, 30, 40, 50].
2. Prints the number of items in the list.
3. Prints the index of temperature 30.
4. Adds another temperature 60 at the end.
5. Loops through the list of temperatures and converts them from Celsius to
Fahrenheit, printing each value in degrees C and F (use print C,F).

{{< spoiler text="Example solution" >}}
{{< code-download file="/courses/python_introduction/solns/set1_proj1.py" lang="python" >}}
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
{{< code-download file="/courses/python_introduction/solns/set1_proj2.py" lang="python" >}}
{{< /spoiler >}}

## Project 3

Write a program that:

1. Generates a list of temperatures from -40 to 100 inclusive by increments of 5 degrees Celsius. 
2. Creates another list of the corresponding Fahrenheit temperatures. 
3. Creates a list of temperatures in degrees Fahrenheit which are greater than zero but for which the corresponding temperature in Celsius is less than zero. Print the elements of this last list.

{{< spoiler text="Example solution" >}}
{{< code-download file="/courses/python_introduction/solns/set1_proj3.py" lang="python" >}}
{{< /spoiler >}}

## Project 4

Download the file [us-state-capitals.csv](/data/us-state-capitals.csv).  Write a program that will read this file and create a dictionary with the state name as the key and the capital name as the value.  Using your dictionary, print the capitals of Arkansas, Virginia, and Wyoming.

Again using your dictionary, generate a list of all state capitals that begin with the letter 'A'.  Use the list to create a string consisting of these city names separated by a semicolon ;   Open a new file capitals-with-a.txt and write this string to it.

---

# Intermediate Level

## Project 5

Write a program that obtains the sum of the numbers from 1 to some specified positive (>0) integer N. Request the value of N as console input from the user. Your program should catch user inputs that cannot be converted to integers greater than 0.  Do not use the Gauss formula, do this via “brute force.”
Print the number, its sum as obtained from your work, and the correct answer from the Gauss formula sum(N)=N(N+1)/2.  Test your program with N=1, N=25, N=1000.

{{< spoiler text="Example solution" >}}
{{< code-download file="/courses/python_introduction/solns/set1_proj4.py" lang="python" >}}
{{< /spoiler >}}

## Project 6

The Collatz conjecture is a fun little exercise in number theory. 

1. Given a positive integer, if it is odd multiply it by 3 and add 1. If it is even divide by 2. 
2. Repeat this procedure until the result is 1.

The Collatz conjecture is that the sequence will always reach 1. No exceptions have been found...yet.  The number of steps required to reach 1 is called the stopping time.

**A.** Write a program that will find and print the stopping time for the first N positive integers. Count the starting number itself as one of the steps. Print a table of N and stopping time.
Test your program for N=30 and N=50.

**B.** Modify your program to print the starting number, its stopping time, and the maximum value of the sequence of numbers. **Hint:** If you use a list you will be able to use the len() and max() intrinsic (built-in) functions. Confirm that you get the same stopping numbers as before.

{{< spoiler text="Example solution" >}}
{{< code-download file="/courses/python_introduction/solns/collatz.py" lang="python" >}}
{{< /spoiler >}}

## Project 7

Write a program to analyze a DNA sequence file.  The program should contain a function `countBases` which takes a sequence consisting of letters ATCG, with each letter the symbol for a base, and returns a dictionary where the key is the letter and the value is the number of times it appears in the sequence.
The program should contain another function `printBaseComposition` which takes the dictionary and prints a table of the proportions of each base, e.g.
-  A : 0.25
-  T : 0.25
-  C : 0.25
-  G : 0.25
Use the file [HIV.txt](/data/HIV.txt) to test your program.  You can look at the file in a text editor.  It consists of a label followed by a space followed by a sequence, for each sequence in the file.
Hints: read each sequence as a line.  Split the line on whitespace (rstrip first) and throw out the 0th element.
Copy the next element of the list into a string, and use substrings to extract each letter.  Build your dictionary as you step through the string.  Repeat for the next line until you have read all the lines.


# Expert Level

## Project 7

In the early 2000’s an “urban legend” circulated that one could read text in which all letters except the first and last were scrambled.  For example:

> Aoccdrnig to rscheearch at an Elingsh uinervtisy, it deosn’t mttaer in waht oredr the ltteers in a wrod are, the olny iprmoetnt tihngis taht the frist and lsat ltteer is at the rghit pclae.

Write a program to scramble a passage input from a file.  Print the result to a file with the same name as the original but a suffix \_scrambled added
(so if the original was Example.txt it will be Example_scrambled.txt).
Look at the scrambled file first—can you read it?
- First write a scramble_word function, then a scramble_line function, and finally a function that writes the lines to the new file.
- Your main() routine will request from the user the name of the input file, then call the printing function, which will call the scramble_line function that will in turn call the scramble_word function.
This is an example of how we divide up work into separate “chunks” or
“concerns.” Internal punctuation (apostrophes) can be scrambled, but leave any
punctuation such as periods, question marks, etc., at the end of a word in
place.
- Hint: since you cannot overwrite strings you will need to convert them to a list and back again. Use [Example.txt](/data/Example.txt) as your sample input.
- FYI this is an “urban legend” because: firstly, no such research was ever conducted at any university, and secondly it is true only for very practiced readers of English and even then only for familiar words that are easy to recognize in context.

--- 

## Project 8

The algorithm for converting a number in base 10 to another base is as follows:

1. Find the remainder of the number divided by the base.
2. Divide the number by the base using integer division. If the result is greater than zero, replace the old value of the number by the result of the integer division and store the remainder previously obtained as the new leftmost digit for the base and repeat. If the result of the integer division is 0, the process is complete.

**A.** Write a program to convert the first 51 integers, starting at 0 and ending at 50, to octal (base 8). Print a table of the decimal number and its octal equivalent.
**Hint:** construct a list of digits as you work through the integer divisions. The elements of the list should be strings so you’ll need to convert from integer to string. To change from a list of individual strings to a single string for printing, use the join function as follows:
 "". join(digits)
That is two (regular, not “smart”) double quotes with nothing between them, followed by a period, followed by join and in parentheses, the name of the list you have created.

**B.** Modify your program to handle bases up to 16 (hexadecimal). Use the letters of the alphabet to represent digits 10, 11, 12, ... as A, B, C, ... Hint: the char(<number>) built-in converts from an integer to its representation in the ASCII collating sequence. Note that A is number 65, i.e. chr(65)="A". The rest of the alphabet follows in numerical sequence to 96, then the lower-case letters begin at 97. Please use upper case letters.
The only widely used base greater than 10 is hexadecimal (base 16). Print a table of 0 to 32 as hexadecimal numbers.  Play with formatting to produce a nice table.

{{< spoiler text="Example solution" >}}
{{< code-download file="/courses/python_introduction/solns/base_convert.py" lang="python" >}}
{{< /spoiler >}}
