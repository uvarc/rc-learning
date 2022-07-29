---
title: Project Set 2
toc: true
type: book
draft: false
weight: 79
---

## Project 6

Write a program that obtains the sum of the numbers from 1 to some specified positive (>0) integer N. Request the value of N as console input from the user. Your program should catch user inputs that cannot be converted to integers greater than 0.  Do not use the Gauss formula, do this via “brute force.”
Print the number, its sum as obtained from your work, and the correct answer from the Gauss formula sum(N)=N(N+1)/2.  Test your program with N=1, N=25, N=1000.

{{< spoiler text="Example solution" >}}
{{< code-download file="/courses/python_introduction/solns/proj5.py" lang="python" >}}
{{< /spoiler >}}

## Project 7

Download the file [us-state-capitals.csv](/data/us-state-capitals.csv).  Write a program that will read this file and create a dictionary with the state name as the key and the capital name as the value.  Using your dictionary, print the capitals of Arkansas, Virginia, and Wyoming.

Again using your dictionary, generate a list of all state capitals that begin with the letter 'A'.  Use the list to create a string consisting of these city names separated by a semicolon ;   Open a new file capitals-with-a.txt and write this string to it.

{{< spoiler text="Example solution" >}}
{{< code-download file="/courses/python_introduction/solns/state_capitals.py" lang="python" >}}
{{< /spoiler >}}

## Project 8

Write a program to analyze a DNA sequence file.  The program should contain at minimum a function `countBases` which takes a sequence consisting of letters ATCG, with each letter the symbol for a base, and returns a dictionary where the key is the letter and the value is the number of times it appears in the sequence.
The program should contain another function `printBaseComposition` which takes the dictionary and prints a table of the proportions of each base, e.g.
-  A : 0.25
-  T : 0.25
-  C : 0.25
-  G : 0.25
Use the file [HIV.txt](/data/HIV.txt) to test your program.  You can look at the file in a text editor.  It consists of a label followed by a space followed by a sequence, for each sequence in the file.
Hints: read each sequence as a line.  Split the line on whitespace (rstrip first) and throw out the 0th element.
Copy the next element of the list into a string, and use substrings to extract each letter.  Build your dictionary as you step through the string.  Repeat for the next line until you have read all the lines.

{{< spoiler text="Example solution" >}}
{{< code-download file="/courses/python_introduction/solns/DNA.py" lang="python" >}}
{{< /spoiler >}}

## Project 9

In the early 2000’s an “urban legend” circulated that one could read text in which all letters except the first and last were scrambled.  For example:

> Aoccdrnig to rscheearch at an Elingsh uinervtisy, it deosn’t mttaer in waht oredr the ltteers in a wrod are, the olny iprmoetnt tihng is taht the frist and lsat ltteer is at the rghit pclae.

Write a program to scramble a passage input from a file.  Print the result to a file with the same name as the original but a suffix \_scrambled added
(so if the original was Example.txt it will be Example_scrambled.txt).
Look at the scrambled file first—can you read it?
- First write a scramble_word function, then a scramble_line function, and finally a function that writes the lines to the new file.
- Your main() routine will request from the user the name of the input file, then call the printing function, which will call the scramble_line function that will in turn call the scramble_word function.
This is an example of how we divide up work into separate “chunks” or
“concerns.” Internal punctuation (apostrophes) can be scrambled, but leave any
punctuation such as periods, question marks, etc., at the end of a word in
place.

Look up the documentation for the random module to find some useful functions.  You can also import the string module to obtain a list of punctuaton marks.

- Hint: since you cannot overwrite strings you will need to convert them to a list and back again. Use [Example.txt](/data/Example.txt) as your sample input.
- FYI this is an “urban legend” because: firstly, no such research was ever conducted at any university, and secondly it is true only for very practiced readers of English and even then only for familiar words that are easy to recognize in context.

{{< spoiler text="Example solution" >}}
{{< code-download file="/courses/python_introduction/solns/scramble_text.py" lang="python" >}}
{{< /spoiler >}}
