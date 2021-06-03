---
title: Exercises - Day 1
toc: true
type: docs
draft: false
weight: 75
menu:
  python_introduction:
    parent: Introduction to Programming in Python
    weight: 75
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

<details>
<summary>See the solution here:</summary>
<pre>
c_temps = [0, 10, 20, 30, 40, 50]

print (f"Number of items: {len(c_temps)}.")

print (f"Index of '30': {c_temps.index(30)}.")

c_temps.append(60)

for t in c_temps:
    f = t*1.8 + 32
    print (f"Fahrenheit: {f:.2f}")
</pre>
</details>

## Project 2

Write a program that:

1. Creates the temperature list 0 through 60 by 10 using a loop rather than by
typing in all the values.
2. As each degree C value is added, converts it to F and adds the F value to a list
for the Fahrenheit equivalents.
3. Makes another loop which prints out C and F similarly to Project 1, i.e. both
on the same line, but does it by indexing into the two lists.

<details>
<summary>See the solution here:</summary>
<pre>
c_temps = []
f_temps = []
for t in range(0,70,10):
    c_temps.append(t)
    f = t * 1.8 + 32
    f_temps.append(f)
for index in range(len(c_temps)):
    print (f"Celsius: {c_temps[index]:.1f}, Fahrenheit: {f_temps[index]:.1f}")
</pre>
</details>

## Project 3

Write a program that:

1. Generates a list of temperatures from -40 to 100 inclusive by increments of 5 degrees Celsius. 
2. Creates another list of the corresponding Fahrenheit temperatures. 
3. Creates a list of temperatures in degrees Fahrenheit which are greater than zero but for which the corresponding temperature in Celsius is less than zero. Print the elements of this last list.

<details>
<summary>See a solution here:</summary>
<pre>
c_temps = []
f_temps = []
for t in range(-40,105,5):
    c_temps.append(t)
    f = t * 1.8 + 32
    f_temps.append(f)

filtered = []
for index in range(len(c_temps)):
    print (c_temps[index], f_temps[index])
    if c_temps[index]<0 and f_temps[index]>0:
        filtered.append(f_temps[index])

print (f"Filtered: {filtered}") 
</pre>
</details>

---

# Intermediate Level

## Project 4

Write a program that obtains the sum of the numbers from 1 to some specified positive (>0) integer N. Request the value of N as console input from the user. Your program should catch user inputs that cannot be converted to integers greater than 0.  Do not use the Gauss formula, do this via “brute force.”
Print the number, its sum as obtained from your work, and the correct answer from the Gauss formula sum(N)=N(N+1)/2.  Test your program with N=1, N=25, N=1000.

<details>
<summary>See solution here:</summary>
<pre>
<code>
n_str = input("Please enter integer number N > 0: ")
try:
    N = int(n_str)
    if N >0:
        b_sum = 0
        for number in range(1,N+1):
            b_sum = b_sum+number
        print (f"Sum (brute force): {b_sum}, sum (Gaussian method): {N*(N+1)//2}.")
    else:
        print ("Please enter an integer number greater than 0.")
except:
    print (f"The entered value {n_str} cannot be converted to an integer number")
</code>
</pre>
</details>

## Project 5

The Collatz conjecture is a fun little exercise in number theory. 

1. Given a positive integer, if it is odd multiply it by 3 and add 1. If it is even divide by 2. 
2. Repeat this procedure until the result is 1.

The Collatz conjecture is that the sequence will always reach 1. No exceptions have been found...yet.  The number of steps required to reach 1 is called the stopping time.

**A.** Write a program that will find and print the stopping time for the first N positive integers. Count the starting number itself as one of the steps. Print a table of N and stopping time.
Test your program for N=30 and N=50.

**B.** Modify your program to print the starting number, its stopping time, and the maximum value of the sequence of numbers. **Hint:** If you use a list you will be able to use the len() and max() intrinsic (built-in) functions. Confirm that you get the same stopping numbers as before.

<details>
<summary>See solution here:</summary>
<pre>
def collatz(N):
    """Collatz conjecture algorithm."""
    steps = [N]
    while N>1:
        if (N % 2 == 0):
            N = N // 2
        else:
            N = N * 3 + 1
        steps.append(N)
    return len(steps),max(steps)
numbers = [30,50]
for N in numbers:
    print (f"Computing Collatz for N={N}")
    header = f"{'N':>5}|{'stopping time':>15}|{'max value':>10}"
    print (header)
    print ("".join(['-'] * len(header)))
    for n in range(1,N+1):
        stop,max_value = collatz(n)
        print (f"{n:5}|{stop:15d}|{max_value:10d}")
</pre>
</details>

--- 

# Expert Level

## Project 6

The algorithm for converting a number in base 10 to another base is as follows:

1. Find the remainder of the number divided by the base.
2. Divide the number by the base using integer division. If the result is greater than zero, replace the old value of the number by the result of the integer division and store the remainder previously obtained as the new leftmost digit for the base and repeat. If the result of the integer division is 0, the process is complete.

**A.** Write a program to convert the first 51 integers, starting at 0 and ending at 50, to octal (base 8). Print a table of the decimal number and its octal equivalent.
**Hint:** construct a list of digits as you work through the integer divisions. The elements of the list should be strings so you’ll need to convert from integer to string. To change from a list of individual strings to a single string for printing, use the join function as follows:
 "". join(digits)
That is two (regular, not “smart”) double quotes with nothing between them, followed by a period, followed by join and in parentheses, the name of the list you have created.

<details>
<summary>See solution here:</summary>
<pre>
def convert_8(N, base=8):
    """Convert base 10 number to number with base 8 (octal)."""
    digits = []
    if n==0:
        return "0"
    while N>0:
        r = N % base
        N = N // base
        digits.append(str(r))
    return "".join(digits[::-1])
print (f"{'Base 10':>8}|{'Base 8':>8}")
for n in range(51):
    print (f"{n:8d}|{convert_8(n):>8}")
</pre>
</details>    
    
**B.** Modify your program to handle bases up to 16 (hexadecimal). Use the letters of the alphabet to represent digits 10, 11, 12, ... as A, B, C, ... Hint: the char(<number>) built-in converts from an integer to its representation in the ASCII collating sequence. Note that A is number 65, i.e. chr(65)="A". The rest of the alphabet follows in numerical sequence to 96, then the lower-case letters begin at 97. Please use upper case letters.
The only widely used base greater than 10 is hexadecimal (base 16). Print a table of 0 to 50 as hexadecimal numbers.

<details>
<summary>See solution here:</summary>
<pre>
def convert(N, alphabet, base=16):
    """Convert base 10 number to number with custom base.""" 
    digits = []
    if n==0:
        return "0"
    while N>0:
        r = N % base
        N = N // base
        digits.append(alphabet[r])
    return "".join(digits[::-1])
base = 8
alphabet = {i:chr(48+i) for i in range(10)}
alphabet.update({10+i:chr(65+i) for i in range(6)})
print (f"{'Base 10':>8}|{'Base {base}':>8}")
for n in range(51):
    conv = convert(n, alphabet, base=base)
    print (f"{n:8d}|{conv:>8}")
</pre>
</details>   