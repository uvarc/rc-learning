"""
Created on Mon Feb 3 17:56:40 2020

This program calculates the Fibonacci numbers for a range of integers.
The used algorithm is inefficient and chosen on purpose to demonstrate the
use of a Profiler.

@author: Karsten Siller
"""
def fib(n):
    n1=0; n2=1;
    if n < 2:
        return n
    else:
        i=1
        while i<n:
            fibb=n1+n2
            n1=n2
            n2=fibb
            i+=1
        return n2

def fib_seq(n):
    results = [ ]
    if n > 0:
        results.extend(fib_seq(n-1))
    results.append(fib(n))
    return results

if __name__ == '__main__':
    print (fib_seq(5))
