#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Mon Feb 3 17:56:40 2020

@author: Katherine Holcomb
"""

import pstats, cProfile

def fib(n):
    # from http://en.literateprograms.org/Fibonacci_numbers_(Python)
    if n == 0:
        return 0
    elif n == 1:
        return 1
    else:
        return fib(n-1) + fib(n-2)

def fib_seq(n):
    results = [ ]
    if n > 0:
        results.extend(fib_seq(n-1))
    results.append(fib(n))
    return results

if __name__ == '__main__':
    # run the profiler and save results in Profile.result
    cProfile.runctx("fib_seq", globals(), {'n':20}, "Profile.result")
    # read profiling stats form Profile.result and print sorted by time
    s = pstats.Stats("Profile.result") 
    s.strip_dirs().sort_stats("time").print_stats()
    
