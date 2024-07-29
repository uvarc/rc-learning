---
date: "2021-04-05"
title: "Project 1"
weight: 32
---

Write a program to compute the day of the week for any date of the Gregorian calendar. Here is the formula: 
```
W=(C+Y+L+M+D ) mod 7 
```
Y is the last two digits of the actual year and D is the actual day. 
You need to obtain the value of C from the following rule for the years: 
* If year is in the 1400s, 1800s, 2200s, C=2 
* If year is in the 1500s, 1900s, 2300s, C=0
* If year is in the 1600s, 2000s, 2400s, C=5 
* If year is in the 1700s, 2100s, 2500s, C=4 
Months are numbered from 1 in the usual way, but (from January) M is 0, 3, 3, 6, 1, 4, 6, 2, 5, 0, 3, 5 

The only tricky part of this algorithm is L, the number of leap days that have occurred since the beginning of the century of the given date. 
To obtain this:
1. Integer divide the last two digits of the year by 4 to obtain the number of “ordinary” leap years in the century up to that year, not counting the century year itself if applicable. 
2. Obtain the remainder of the two digits and 4. If it is not a century year and the remainder is 0 the year is a leap year, otherwise it is not. If the year itself is a century year see Step 3. 
3. If the century (1400, 1500, etc.) was evenly divisible by 400 then the century year is a leap year, otherwise it is not. Thus 2000 was a leap year but 1900 was not. So add 1 for centuries divisible by 400 and 0 otherwise. 
4. If your date is January 1-February 29 of a leap year, subtract 1. 
Try to devise a method to obtain the last two digits on your own. Print the day of the week as a word (Monday, Tuesday, etc.). Remember that Sunday is the first day of the week and it will be counted as 0 in this algorithm. 

Test your program first with your own birthdate. Then test with the following dates: 
* Today’s date 
* December 25, 1642 (Note: this is Newton’s birthdate in the Julian calendar, but use it as a Gregorian date) 
* October 12, 1492 
* January 20, 2000 
* December 11, 2525

Try to write and test your own program before peeking at the sample solution.

{{< spoiler text="Sample solution" >}}
{{< code-download file="/courses/fortran-introduction/solns/day_of_week.f90" lang="fortran" >}}
{{< /spoiler >}}
