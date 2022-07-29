"""
Created on Mon Feb 3 17:56:40 2020

This program calculates the Fibonacci numbers for a range of integers.
The used algorithm is inefficient and chosen on purpose to demonstrate the
use of a Profiler.

@author: Karsten Siller
"""
def fib(n):
    # from http://en.literateprograms.org/Fibonacci_numbers_(Python)
    if n < 2:
        return n
    else:
        return fib(n-1) + fib(n-2)

def fib_seq(n):
    results = [ ]
    if n > 0:
        results.extend(fib_seq(n-1))
    results.append(fib(n))
    return results

if __name__ == '__main__':
    print (fib_seq(30))
